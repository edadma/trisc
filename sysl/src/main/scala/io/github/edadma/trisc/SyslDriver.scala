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

object SyslDriver:
  /** Marker file whose presence in a directory designates that directory as a
   *  sysl project root. Module paths for source files under such a directory
   *  are computed *relative to the project root* rather than relative to the
   *  filesystem root, so a file at `<root>/parsyl/parsyl.lsysl` can declare
   *  `module parsyl` regardless of where `<root>` lives on disk. v1 only uses
   *  the marker's existence; future versions may parse its contents for
   *  package metadata. */
  val ProjectMarker: String = "sysl.toml"

  /** Walk up from `filePath`'s parent directory looking for `marker` (default
   *  `ProjectMarker`). Returns the directory path containing the marker, or
   *  None if no marker is found between the file and the filesystem root.
   *  Works for both absolute and relative paths; relative paths are resolved
   *  against the cwd via the underlying `FileOps.exists`. */
  def findProjectRoot(io: FileOps, filePath: String, marker: String = ProjectMarker): Option[String] =
    def parent(p: String): Option[String] =
      val stripped = p.stripSuffix("/")
      val i = stripped.lastIndexOf('/')
      if i < 0 then if stripped.isEmpty then None else Some("")
      else if i == 0 then if stripped == "/" then None else Some("/")
      else Some(stripped.substring(0, i))
    @scala.annotation.tailrec
    def walk(dir: String): Option[String] =
      val markerHere = if dir.isEmpty then marker else io.joinPath(dir, marker)
      if io.exists(markerHere) then Some(if dir.isEmpty then "." else dir)
      else parent(dir) match
        case None => None
        case Some(p) => walk(p)
    parent(filePath).flatMap(walk)

  /** Compute the source-map key for a file at `filePath` given an optional
   *  explicit `baseDir`. An explicit `baseDir` (non-empty) is the base; with
   *  no `baseDir`, falls back to project-marker discovery and then to the
   *  full slash-stripped path. The returned key is the path *without* the
   *  `.sysl` / `.lsysl` extension. */
  def computeSourceKey(io: FileOps, filePath: String, baseDir: String = ""): String =
    val name = io.fileName(filePath)
    val effectiveBase =
      if baseDir.nonEmpty then baseDir
      else findProjectRoot(io, filePath) match
        case Some(root) =>
          val r = if root == "." then "" else root
          if r.isEmpty || r.endsWith("/") then r else r + "/"
        case None => ""
    val relPath = if effectiveBase.nonEmpty && filePath.startsWith(effectiveBase) then
      val rel = filePath.drop(effectiveBase.length).dropWhile(c => c == '/' || c == '\\')
      if rel.nonEmpty then rel else name
    else if effectiveBase.isEmpty then
      val rel = filePath.dropWhile(c => c == '/' || c == '\\')
      if rel.nonEmpty then rel else name
    else name
    if relPath.endsWith(".lsysl") then relPath.stripSuffix(".lsysl")
    else relPath.stripSuffix(".sysl")

class SyslDriver(fileOps: Option[FileOps] = None, baseDirs: List[String] = Nil, config: Map[String, String] = Map.empty, tangler: Option[String => String] = None):

  case class DriverError(msg: String) extends RuntimeException(msg)

  /** `--no-contracts` build flag: strip all runtime contract checks
   *  (require/ensure/invariant/variant/struct-invariants/type-predicates/type-attribute
   *  traps). Set via `config("contracts") = "off"` or `= "false"`. Ada pragma
   *  Assertion_Policy(Disable) equivalent — the user takes responsibility for
   *  correctness in exchange for no runtime overhead. */
  val contractsEnabled: Boolean = config.get("contracts") match
    case Some("off") | Some("false") | Some("disabled") => false
    case _ => true

  def compile(sources: Map[String, String], keepTests: Boolean = false): CompilationResult =
    // Step 1: Parse all sources
    val parsedAsts = parseSources(sources)

    // Step 2: Resolve conditional compilation
    val asts = parsedAsts.map((name, ast) => (name, resolveCondDecls(ast)))

    // Step 3: Extract imports and module declarations
    val imports = extractImports(asts)
    val modules = extractModules(asts)

    // Step 3b: Validate module declarations match file paths
    // Relaxed: allows a module to declare a shorter path that is a prefix of the
    // directory path (e.g. oskit/arch/x86_64/cpu declaring "module oskit.arch").
    // This supports arch modules that declare a platform-neutral module path so
    // the kernel can import from oskit.arch.* regardless of which platform's
    // directory is compiled. TODO: revisit — may want a proper module-map flag.
    for (name, modPath) <- modules do
      val dirPath = name.lastIndexOf('/') match
        case -1 => ""
        case i => name.substring(0, i)
      val expectedModPath = dirPath.replace('/', '.')
      val actualModPath = modPath.replace("/", ".")
      val fullFilePath = name.replace('/', '.')
      if actualModPath != expectedModPath && !expectedModPath.startsWith(actualModPath) && actualModPath != fullFilePath then
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
    //
    // Modules are processed in dependency order derived from the file-level
    // topological sort, so cross-module imports are available during pre-collection.
    val packageMetaCache = new mutable.LinkedHashMap[String, ModuleMeta]
    // Build module-level dependency graph: for each module, collect all modules
    // imported by any of its files.
    // Resolve an import's module path to a module key in moduleToSources,
    // handling QualifiedImport where "std/mem/memset" should resolve to "std/mem".
    def resolveImportToModule(imp: ImportDeclAST): String =
      imp.selectors match
        case List(QualifiedImport) =>
          val path = imp.modulePath
          if moduleToSources.contains(path) then path
          else
            val parts = path.split("/")
            if parts.length >= 2 then parts.init.mkString("/") else path
        case _ => imp.modulePath

    val moduleDeps: Map[String, Set[String]] = moduleToSources.map { (modPath, srcNames) =>
      val deps = srcNames.flatMap { name =>
        imports.getOrElse(name, Nil).map(resolveImportToModule)
      }.filter(p => moduleToSources.contains(p) && p != modPath)
      (modPath, deps)
    }
    // Topological sort of modules
    val moduleVisited = mutable.LinkedHashSet[String]()
    def visitModule(mp: String): Unit =
      if !moduleVisited.contains(mp) then
        for depMod <- moduleDeps.getOrElse(mp, Set.empty) do visitModule(depMod)
        moduleVisited += mp
    for mp <- moduleToSources.keys do visitModule(mp)
    val moduleOrder = moduleVisited.toList
    for modPath <- moduleOrder do
      val sourceNames = moduleToSources(modPath)
      var meta = new ModuleMeta(Nil)
      var remaining = sourceNames.toList
      var changed = true
      while changed && remaining.nonEmpty do
        changed = false
        val stillFailing = new mutable.ListBuffer[String]
        for name <- remaining do
          scala.util.Try {
            val ast = if keepTests then asts(name)
              else ProgramAST(asts(name).decls.filter {
                case f: FunDeclAST => !f.attributes.exists(_.name == "test")
                case _ => true
              })
            val analyzer = new SyslAnalyzer(contractsEnabled = contractsEnabled)
            // Register extern names so they are never mangled (ABI-level symbols)
            analyzer.registerNoMangle(globalExternNames)
            for src <- sourceNames if src != name do
              analyzer.registerGenericTemplatesFrom(asts(src))
            val siblings = new ModuleMeta(meta.symbols.filter(s => !s.isExtern))
            analyzer.registerImport(siblings)
            // Register cross-module imports from already-cached modules,
            // applying the same QualifiedImport → NamedImport fallback as Step 5.
            for imp0 <- imports.getOrElse(name, Nil) do
              val imp = imp0.selectors match
                case List(QualifiedImport) =>
                  if packageMetaCache.contains(imp0.modulePath) then imp0
                  else
                    val parts = imp0.modulePath.split("/")
                    if parts.length >= 2 then
                      ImportDeclAST(parts.init.mkString("/"), List(NamedImport(parts.last)))
                    else imp0
                case _ => imp0
              if packageMetaCache.contains(imp.modulePath) then
                analyzer.registerImport(packageMetaCache(imp.modulePath), imp.selectors, imp.modulePath)
            val typed = analyzer.analyze(ast)
            val baseMeta = ModuleMeta.fromProgram(typed, Some(s"$name.sysl"))
            // Carry the analyzer's per-file extension entries through so sibling
            // files in the same module see them via the next iteration's
            // registerImport(siblings) call.
            new ModuleMeta(
              baseMeta.symbols,
              Nil,
              analyzer.getTraitImplMetas,
              analyzer.getGenericEnumInstances,
              analyzer.getExtensionMetas,
            )
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
      // Strip #test functions unless keepTests is set (for test runner)
      val ast = if keepTests then asts(name)
        else ProgramAST(asts(name).decls.filter {
          case f: FunDeclAST => !f.attributes.exists(_.name == "test")
          case _ => true
        })
      val analyzer = new SyslAnalyzer(contractsEnabled = contractsEnabled)

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

      // Phase 2b-Predef-auto-import: silently inject an extensions-only import
      // for every Predef module that's present in the meta cache (or
      // resolvable via resolveExternalMeta) and isn't the unit's own module.
      // This is what makes `"hi".chars` work without a literal
      // `import std.string`. Only the extension entries + their `__ext_*` synth
      // functions are pulled in — regular functions (e.g. `contains`) stay
      // out of the importing unit's namespace so they don't clash with
      // same-named functions in other modules. If the Predef module isn't in
      // the source set, this is a no-op (no error).
      val ownModulePath: Option[String] = modules.get(name)
      val explicitImportPaths: Set[String] = imports(name).map(_.modulePath).toSet
      for predef <- analyzer.predefModulePaths do
        if !ownModulePath.contains(predef) && !explicitImportPaths.contains(predef) then
          if packageMetaCache.contains(predef) then
            analyzer.registerImport(packageMetaCache(predef), List(ExtensionsOnlyImport), predef)
          else if smetaCache.contains(predef) then
            ModuleMeta.fromSmeta(smetaCache(predef)).foreach(
              analyzer.registerImport(_, List(ExtensionsOnlyImport), predef))
          else
            resolveExternalMeta(predef) match
              case Some(meta) =>
                packageMetaCache(predef) = meta
                analyzer.registerImport(meta, List(ExtensionsOnlyImport), predef)
              case None => ()

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
      // Extract generic templates and trait declarations from the source AST.
      // `analyzer.getExtensionTemplates` adds generic-receiver extension synth
      // FunDecls (Phase 2d) — these come from `lowerExtensions` and aren't in
      // `ast.decls` directly.
      val templates = ast.decls.filter {
        case StructDeclAST(_, _, tps, _, _) => tps.nonEmpty
        case DataEnumDeclAST(_, _, tps, _) => tps.nonEmpty
        case FunDeclAST(_, _, _, _, _, tps, _, _, _, _) => tps.nonEmpty
        case _ => false
      } ++ analyzer.getTraitDecls ++ analyzer.getExtensionTemplates ++ analyzer.getExtensionImplDecls
      val baseMeta = ModuleMeta.fromProgram(typed, if modPath.isDefined then Some(s"$name.sysl") else None)
      val meta = new ModuleMeta(baseMeta.symbols, templates, analyzer.getTraitImplMetas, analyzer.getGenericEnumInstances, analyzer.getExtensionMetas)
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
          case StructDeclAST(_, _, tps, _, _) => tps.nonEmpty
          case DataEnumDeclAST(_, _, tps, _) => tps.nonEmpty
          case FunDeclAST(_, _, _, _, _, tps, _, _, _, _) => tps.nonEmpty
          case _ => false
        }
        scala.util.Try {
          val analyzer = new SyslAnalyzer(contractsEnabled = contractsEnabled)
          val typed = analyzer.analyze(stripped)
          val meta = ModuleMeta.fromProgram(typed)
          new ModuleMeta(meta.symbols, templates ++ analyzer.getTraitDecls, analyzer.getTraitImplMetas, analyzer.getGenericEnumInstances, analyzer.getExtensionMetas)
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
