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
)

case class CompilationResult(
    units: List[CompilationUnit],
    order: List[String],
)

class SyslDriver:

  case class DriverError(msg: String) extends RuntimeException(msg)

  def compile(sources: Map[String, String]): CompilationResult =
    // Step 1: Parse all sources
    val asts = parseSources(sources)

    // Step 2: Extract imports, build dependency graph
    val imports = extractImports(asts)

    // Step 3: Topological sort
    val order = topologicalSort(imports, sources.keySet)

    // Step 4: Compile in order
    val smetaCache = new mutable.LinkedHashMap[String, String]
    val units = new mutable.ListBuffer[CompilationUnit]

    for name <- order do
      val ast = asts(name)
      val analyzer = new SyslAnalyzer

      // Register imports from previously compiled modules
      for imp <- imports(name) do
        smetaCache.get(imp) match
          case Some(smeta) => analyzer.registerImport(ModuleMeta.fromSmeta(smeta))
          case None => throw DriverError(s"$name: import '$imp' not found (not in source set)")

      val typed = analyzer.analyze(ast)
      val meta = ModuleMeta.fromProgram(typed)
      val smeta = meta.toSmeta

      smetaCache(name) = smeta
      units += CompilationUnit(name, sources(name), ast, typed, meta, smeta, analyzer.externals)

    CompilationResult(units.toList, order)

  def parseSources(sources: Map[String, String]): Map[String, ProgramAST] =
    sources.map { (name, source) =>
      val parser = new SyslParser
      parser.parseProgram(source) match
        case Right(ast) => (name, ast)
        case Left(err) => throw DriverError(s"$name: parse error: $err")
    }

  def extractImports(asts: Map[String, ProgramAST]): Map[String, List[String]] =
    asts.map { (name, ast) =>
      val imports = ast.decls.collect { case ImportDeclAST(path) => path }
      (name, imports)
    }

  def topologicalSort(imports: Map[String, List[String]], allNames: Set[String]): List[String] =
    val visited = new mutable.LinkedHashSet[String]
    val visiting = new mutable.LinkedHashSet[String]
    val result = new mutable.ListBuffer[String]

    def visit(name: String): Unit =
      if visiting.contains(name) then
        throw DriverError(s"circular dependency involving '$name'")
      if !visited.contains(name) then
        visiting += name
        for dep <- imports.getOrElse(name, Nil) if allNames.contains(dep) do
          visit(dep)
        visiting -= name
        visited += name
        result += name

    for name <- allNames do visit(name)
    result.toList
