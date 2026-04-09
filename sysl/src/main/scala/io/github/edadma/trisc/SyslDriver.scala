package io.github.edadma.trisc

import scala.collection.mutable

case class CompilationUnit(
    name: String,
    source: String,
    ast: ProgramAST,
    typed: TProgram,
    meta: ModuleMeta,
    smeta: String,
    externals: Set[String],
    modulePath: Option[String] = None,
)

case class CompilationResult(
    units: List[CompilationUnit],
    order: List[String],
    packageMetas: Map[String, ModuleMeta] = Map.empty,
)

class SyslDriver(fileOps: Option[FileOps] = None, baseDirs: List[String] = Nil, config: Map[String, String] = Map.empty, tangler: Option[String => String] = None):

  case class DriverError(msg: String) extends RuntimeException(msg)

  def compile(sources: Map[String, String]): CompilationResult =
    // Step 1: Parse all sources
    val parsedAsts = parseSources(sources)

    // Step 2: Resolve conditional compilation
    val asts = parsedAsts.map((name, ast) => (name, resolveCondDecls(ast)))

    // Step 3: Extract imports and module declarations
    val imports = extractImports(asts)
    val modules = extractModules(asts)

    // Step 3b: Validate module declarations match file paths
    for (name, modPath) <- modules do
      val dirPath = name.lastIndexOf('/') match
        case -1 => ""
        case i => name.substring(0, i)
      val expectedModPath = dirPath.replace('/', '.')
      val actualModPath = modPath.replace("/", ".")
      if actualModPath != expectedModPath then
        throw DriverError(
          s"$name: module declaration 'module $actualModPath' does not match directory path" +
            (if expectedModPath.isEmpty then " (expected no module declaration for top-level file)"
            else s" (expected 'module $expectedModPath')")
        )

    // Step 4: Topological sort
    // Build reverse map: module path → set of source names that belong to it
    val moduleToSources = modules.groupMap(_._2)(_._1).map((k, v) => (k, v.toSet))
    val order = topologicalSort(imports, sources.keySet, moduleToSources)

    // Step 4b-pre: Collect all extern function names across all sources.
    // Extern declarations are ABI promises — the named function must be
    // linkable by its unmangled name, even if defined in a different module.
    val globalExternNames: Set[String] =
      asts.values.flatMap(_.decls.collect {
        case ExternFuncDeclAST(name, _, _, _) => name
        case ExternVarDeclAST(name, _, _) => name
      }).toSet

    // Step 4b: Pre-collect declarations for intra-module visibility
    // Iteratively analyze module files to extract declarations. Each round
    // makes previously collected symbols available to unresolved files.
    // Converges when no new files succeed or all are resolved.
    val packageMetaCache = new mutable.LinkedHashMap[String, ModuleMeta]
    for (modPath, sourceNames) <- moduleToSources do
      var meta = new ModuleMeta(Nil)
      var remaining = sourceNames.toList
      var changed = true
      while changed && remaining.nonEmpty do
        changed = false
        val stillFailing = new mutable.ListBuffer[String]
        for name <- remaining do
          scala.util.Try {
            val ast = asts(name)
            val analyzer = new SyslAnalyzer
            // Register extern names so they are never mangled (ABI-level symbols)
            analyzer.registerNoMangle(globalExternNames)
            for src <- sourceNames if src != name do
              analyzer.registerGenericTemplatesFrom(asts(src))
            val siblings = new ModuleMeta(meta.symbols.filter(s => !s.isExtern))
            analyzer.registerImport(siblings)
            val typed = analyzer.analyze(ast)
            ModuleMeta.fromProgram(typed, Some(s"$name.sysl"))
          } match
            case scala.util.Success(fileMeta) =>
              meta = meta.merge(fileMeta)
              changed = true
            case scala.util.Failure(_) =>
              stillFailing += name
        remaining = stillFailing.toList
      packageMetaCache(modPath) = meta

    // Step 5: Compile in order
    val smetaCache = new mutable.LinkedHashMap[String, String]
    val units = new mutable.ListBuffer[CompilationUnit]

    for name <- order do
      val ast = asts(name)
      val analyzer = new SyslAnalyzer

      // Register extern names so they are never mangled (ABI-level symbols)
      analyzer.registerNoMangle(globalExternNames)

      // Register same-module siblings (intra-module visibility),
      // excluding own symbols and externs (which are private to each file).
      for modPath <- modules.get(name) do
        for src <- moduleToSources.getOrElse(modPath, Set.empty) if src != name do
          analyzer.registerGenericTemplatesFrom(asts(src))
        packageMetaCache.get(modPath).foreach { meta =>
          val siblings = new ModuleMeta(meta.symbols.filter(s => s.sourceFile != Some(s"$name.sysl") && !s.isExtern))
          analyzer.registerImport(siblings)
        }

      // Register imports from previously compiled modules (or stdlib)
      for imp0 <- imports(name) do
        // Resolve QualifiedImport ambiguity: import std.strings could be a qualified
        // module import (access as strings.foo) or a single-symbol import (import "strings" from "std").
        // Try full path as module first; if not found, fall back to last-segment-as-name.
        val imp = imp0.selectors match
          case List(QualifiedImport) =>
            def isKnownModule(path: String): Boolean =
              smetaCache.contains(path) ||
                packageMetaCache.contains(path) || resolveExternalMeta(path).isDefined
            if isKnownModule(imp0.modulePath) then imp0 // full path is a module → qualified import
            else
              // Fall back: treat last segment as a named import from parent path
              val parts = imp0.modulePath.split("/")
              if parts.length >= 2 then
                ImportDeclAST(parts.init.mkString("/"), List(NamedImport(parts.last)))
              else imp0
          case _ => imp0
        if packageMetaCache.contains(imp.modulePath) then
          analyzer.registerImport(packageMetaCache(imp.modulePath), imp.selectors, imp.modulePath)
        else if smetaCache.contains(imp.modulePath) then
          ModuleMeta.fromSmeta(smetaCache(imp.modulePath)).foreach(analyzer.registerImport(_, imp.selectors, imp.modulePath))
        else
          // Try resolving from file system
          resolveExternalMeta(imp.modulePath) match
            case Some(meta) =>
              packageMetaCache(imp.modulePath) = meta
              analyzer.registerImport(meta, imp.selectors, imp.modulePath)
            case None =>
              throw DriverError(s"$name: import '${imp.modulePath}' not found (not in source set)")

      val typed = analyzer.analyze(ast)
      val modPath = modules.get(name)
      // Extract generic templates and trait declarations from the source AST
      val templates = ast.decls.filter {
        case StructDeclAST(_, _, tps, _) => tps.nonEmpty
        case DataEnumDeclAST(_, _, tps, _) => tps.nonEmpty
        case FunDeclAST(_, _, _, _, _, tps, _, _, _) => tps.nonEmpty
        case _ => false
      } ++ analyzer.getTraitDecls
      val baseMeta = ModuleMeta.fromProgram(typed, if modPath.isDefined then Some(s"$name.sysl") else None)
      val meta = new ModuleMeta(baseMeta.symbols, templates, analyzer.getTraitImplMetas, analyzer.getGenericEnumInstances)
      val smeta = meta.toSmeta

      modPath match
        case Some(path) =>
          // File belongs to a package — update the package-level meta
          val existing = packageMetaCache.getOrElse(path, new ModuleMeta(Nil))
          val merged = existing.merge(meta)
          packageMetaCache(path) = merged
          smetaCache(path) = merged.toSmeta
        case None =>
          // Standalone file module
          smetaCache(name) = smeta

      units += CompilationUnit(name, sources(name), ast, typed, meta, smeta, analyzer.externals, modPath)

    CompilationResult(units.toList, order, packageMetaCache.toMap)

  def parseSources(sources: Map[String, String]): Map[String, ProgramAST] =
    sources.map { (name, source) =>
      val parser = new SyslParser
      parser.parseProgram(source) match
        case Right(ast) => (name, ast)
        case Left(err) => throw DriverError(s"$name: parse error: $err")
    }

  def extractImports(asts: Map[String, ProgramAST]): Map[String, List[ImportDeclAST]] =
    asts.map { (name, ast) =>
      val imports = ast.decls.collect { case imp: ImportDeclAST => imp }
      (name, imports)
    }

  /** Extract module declarations: file name → module path (as slash-separated string). */
  def extractModules(asts: Map[String, ProgramAST]): Map[String, String] =
    asts.flatMap { (name, ast) =>
      ast.decls.collectFirst { case ModuleDeclAST(path) => (name, path.mkString("/")) }
    }

  def topologicalSort(imports: Map[String, List[ImportDeclAST]], allNames: Set[String], moduleToSources: Map[String, Set[String]] = Map.empty): List[String] =
    val visited = new mutable.LinkedHashSet[String]
    val visiting = new mutable.LinkedHashSet[String]
    val result = new mutable.ListBuffer[String]

    // For QualifiedImport, the module path might be the full path (e.g., "posix/string/memset").
    // Try the full path first; if not found, try parent path (e.g., "posix/string").
    def resolveDepPath(imp: ImportDeclAST): String =
      imp.selectors match
        case List(QualifiedImport) =>
          val path = imp.modulePath
          if allNames.contains(path) || moduleToSources.contains(path) then path
          else
            val parts = path.split("/")
            if parts.length >= 2 then parts.init.mkString("/") else path
        case _ => imp.modulePath

    def visit(name: String): Unit =
      if visiting.contains(name) then
        throw DriverError(s"circular dependency involving '$name'")
      if !visited.contains(name) then
        visiting += name
        for dep <- imports.getOrElse(name, Nil).map(resolveDepPath).distinct do
          if allNames.contains(dep) then
            visit(dep)
          else
            // Check if the import resolves to a module (folder-based)
            for src <- moduleToSources.getOrElse(dep, Set.empty) do
              visit(src)
        visiting -= name
        visited += name
        result += name

    for name <- allNames do visit(name)
    result.toList

  def collectStdlibImports(units: List[CompilationUnit]): Set[String] =
    units.flatMap(_.typed.decls).collect {
      case TImportDecl(path) if SyslStdlib.builtinModules.contains(path) => path
    }.toSet

  /** Resolve conditional compilation directives in a parsed AST. */
  private def resolveCondDecls(program: ProgramAST): ProgramAST =
    ProgramAST(program.decls.flatMap(resolveDecl))

  private def resolveDecl(decl: DeclAST): List[DeclAST] =
    decl match
      case CondDeclAST(cond, thenDecls, elseDecls) =>
        val active = if evalCond(cond) then thenDecls else elseDecls.getOrElse(Nil)
        active.flatMap(resolveDecl)
      case other => List(other)

  private def evalCond(expr: CondExpr): Boolean =
    expr match
      case CondSymbol(name) =>
        config.get(name) match
          case None => false
          case Some("false") => false
          case Some("0") => false
          case Some("") => false
          case Some(_) => true
      case CondNot(inner) => !evalCond(inner)
      case CondEq(name, value) => config.get(name).contains(value)
      case CondNeq(name, value) => !config.get(name).contains(value)

  private def compileExternalFile(source: String): Option[ModuleMeta] =
    val parser = new SyslParser
    parser.parseProgram(source) match
      case Right(ast) =>
        // Strip imports and test functions — we only need declarations for metadata.
        // This allows .lsysl files that import test utilities to still provide metadata.
        val stripped = ProgramAST(ast.decls.filter {
          case _: ImportDeclAST  => false
          case f: FunDeclAST     => !f.attributes.exists(_.name == "test")
          case _                 => true
        })
        // Extract generic templates (structs, enums, functions with type params)
        val templates = stripped.decls.filter {
          case StructDeclAST(_, _, tps, _) => tps.nonEmpty
          case DataEnumDeclAST(_, _, tps, _) => tps.nonEmpty
          case FunDeclAST(_, _, _, _, _, tps, _, _, _) => tps.nonEmpty
          case _ => false
        }
        scala.util.Try {
          val analyzer = new SyslAnalyzer
          val typed = analyzer.analyze(stripped)
          val meta = ModuleMeta.fromProgram(typed)
          new ModuleMeta(meta.symbols, templates ++ analyzer.getTraitDecls, analyzer.getTraitImplMetas, analyzer.getGenericEnumInstances)
        }.toOption
      case Left(_) => None

  /** Try to resolve an import path from the file system by looking for a .smeta file. */
  private def resolveExternalMeta(modulePath: String): Option[ModuleMeta] =
    fileOps match
      case None => None
      case Some(io) =>
        // Convert module path (e.g., "posix/lib/string") to directory path
        // Try each base directory
        val dirs = if baseDirs.isEmpty then List(".") else baseDirs
        dirs.iterator.flatMap { base =>
          val dirPath = io.joinPath(base, modulePath)
          val smetaPath = io.joinPath(dirPath, ".smeta")
          val filePath = s"${io.joinPath(base, modulePath)}.sysl"

          if io.exists(smetaPath) then
            // Directory with .smeta — returns None if stale version
            ModuleMeta.fromSmeta(io.readFile(smetaPath))
          else if io.exists(filePath) then
            // Single file module — compile it on demand
            compileExternalFile(io.readFile(filePath))
          else
            // Try .lsysl (literate source) — requires tangler
            val lsyslPath = s"${io.joinPath(base, modulePath)}.lsysl"
            if tangler.isDefined && io.exists(lsyslPath) then
              compileExternalFile(tangler.get(io.readFile(lsyslPath)))
            else
              // Try directory with .lsysl files inside
              if tangler.isDefined && io.exists(dirPath) && io.isDirectory(dirPath) then
                val lsyslFiles = io.listFiles(dirPath).filter(_.endsWith(".lsysl"))
                if lsyslFiles.nonEmpty then
                  val metas = lsyslFiles.flatMap { f =>
                    compileExternalFile(tangler.get(io.readFile(f)))
                  }
                  if metas.nonEmpty then Some(metas.reduce(_.merge(_)))
                  else None
                else None
              else None
        }.nextOption()
