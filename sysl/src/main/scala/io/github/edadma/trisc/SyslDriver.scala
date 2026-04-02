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

class SyslDriver(fileOps: Option[FileOps] = None, baseDirs: List[String] = Nil):

  case class DriverError(msg: String) extends RuntimeException(msg)

  def compile(sources: Map[String, String]): CompilationResult =
    // Step 1: Parse all sources
    val asts = parseSources(sources)

    // Step 2: Extract imports and module declarations
    val imports = extractImports(asts)
    val modules = extractModules(asts)

    // Step 3: Topological sort
    val order = topologicalSort(imports, sources.keySet)

    // Step 4: Compile in order
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
      val meta = ModuleMeta.fromProgram(typed, modPath.map(_ => s"$name.sysl"))
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

  def topologicalSort(imports: Map[String, List[ImportDeclAST]], allNames: Set[String]): List[String] =
    val visited = new mutable.LinkedHashSet[String]
    val visiting = new mutable.LinkedHashSet[String]
    val result = new mutable.ListBuffer[String]

    def visit(name: String): Unit =
      if visiting.contains(name) then
        throw DriverError(s"circular dependency involving '$name'")
      if !visited.contains(name) then
        visiting += name
        for dep <- imports.getOrElse(name, Nil).map(_.modulePath).distinct if allNames.contains(dep) do
          visit(dep)
        visiting -= name
        visited += name
        result += name

    for name <- allNames do visit(name)
    result.toList

  def collectStdlibImports(units: List[CompilationUnit]): Set[String] =
    units.flatMap(_.typed.decls).collect {
      case TImportDecl(path) if SyslStdlib.modules.contains(path) => path
    }.toSet

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
