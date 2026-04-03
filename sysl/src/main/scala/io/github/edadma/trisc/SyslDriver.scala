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

class SyslDriver(fileOps: Option[FileOps] = None, baseDirs: List[String] = Nil, config: Map[String, String] = Map.empty):

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

    // Step 5: Compile in order
    val smetaCache = new mutable.LinkedHashMap[String, String]
    val packageMetaCache = new mutable.LinkedHashMap[String, ModuleMeta]
    val units = new mutable.ListBuffer[CompilationUnit]

    for name <- order do
      val ast = asts(name)
      val analyzer = new SyslAnalyzer

      // Register imports from previously compiled modules (or stdlib)
      for imp <- imports(name) do
        if SyslStdlib.modules.contains(imp.modulePath) then
          analyzer.registerImport(SyslStdlib.meta(imp.modulePath), imp.selectors)
        else if smetaCache.contains(imp.modulePath) then
          analyzer.registerImport(ModuleMeta.fromSmeta(smetaCache(imp.modulePath)), imp.selectors)
        else if packageMetaCache.contains(imp.modulePath) then
          analyzer.registerImport(packageMetaCache(imp.modulePath), imp.selectors)
        else
          // Try resolving from file system
          resolveExternalMeta(imp.modulePath) match
            case Some(meta) =>
              packageMetaCache(imp.modulePath) = meta
              analyzer.registerImport(meta, imp.selectors)
            case None =>
              throw DriverError(s"$name: import '${imp.modulePath}' not found (not in source set)")

      val typed = analyzer.analyze(ast)
      val modPath = modules.get(name)
      val meta = ModuleMeta.fromProgram(typed, if modPath.isDefined then Some(s"$name.sysl") else None)
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

    def visit(name: String): Unit =
      if visiting.contains(name) then
        throw DriverError(s"circular dependency involving '$name'")
      if !visited.contains(name) then
        visiting += name
        for dep <- imports.getOrElse(name, Nil).map(_.modulePath).distinct do
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
      case TImportDecl(path) if SyslStdlib.modules.contains(path) => path
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
            // Directory with .smeta
            Some(ModuleMeta.fromSmeta(io.readFile(smetaPath)))
          else if io.exists(filePath) then
            // Single file module — compile it on demand
            val source = io.readFile(filePath)
            val parser = new SyslParser
            parser.parseProgram(source) match
              case Right(ast) =>
                val analyzer = new SyslAnalyzer
                val typed = analyzer.analyze(ast)
                Some(ModuleMeta.fromProgram(typed))
              case Left(_) => None
          else None
        }.nextOption()
