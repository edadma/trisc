package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Measure the size reduction the peephole optimizer produces on a few representative
 *  sysl programs. Not a correctness test — purely diagnostic output. Run with:
 *  `sbt 'syslJVM/testOnly io.github.edadma.trisc.PeepholeMeasure'`. */
class PeepholeMeasure extends AnyFreeSpec with Matchers:

  private def compile(src: String, peephole: Boolean): String =
    val Right(ast) = (new SyslParser).parseProgram(src): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    new SyslTriscCodegen(peepholeEnabled = peephole).generate(typed)

  private def report(name: String, src: String): Unit =
    val raw = compile(src, peephole = false)
    val opt = compile(src, peephole = true)
    val rawLines = raw.linesIterator.toList
    val optLines = opt.linesIterator.toList
    def instrCount(s: String): Int = s.linesIterator.count(_.startsWith("  "))
    val rawI = instrCount(raw)
    val optI = instrCount(opt)
    val pct = if rawI == 0 then 0.0 else 100.0 * (rawI - optI) / rawI
    val (_, stats) = TriscPeephole(raw)
    println(f"\n=== $name ===")
    println(f"raw:  ${rawLines.length}%4d lines, $rawI%4d instructions")
    println(f"opt:  ${optLines.length}%4d lines, $optI%4d instructions  (-${rawI - optI}%d, $pct%.1f%%)")
    println(stats.report)

  "factorial (single recursive inner def)" in {
    report("factorial",
      """outer() -> int
        |    def fact(n: int) -> int
        |        if n == 0 then return 1
        |        n * fact(n - 1)
        |    fact(5)
        |
        |main() -> int = outer()
        |""".stripMargin)
  }

  "sum_with_bonus (capture + recursion)" in {
    report("sum_with_bonus",
      """outer(bonus: int) -> int
        |    def sum_with_bonus(n: int) -> int
        |        if n == 0 then return 0
        |        n + bonus + sum_with_bonus(n - 1)
        |    sum_with_bonus(3)
        |
        |main() -> int = outer(10)
        |""".stripMargin)
  }

  "loop-heavy" in {
    report("loop_heavy",
      """sum_to(n: int) -> int
        |    var total = 0
        |    var i = 0
        |    while i < n
        |        total = total + i
        |        i = i + 1
        |    total
        |
        |main() -> int = sum_to(100)
        |""".stripMargin)
  }

  "struct + field access" in {
    report("struct_fields",
      """struct Point
        |    x: int
        |    y: int
        |
        |dist_sq(p: Point) -> int = p.x * p.x + p.y * p.y
        |
        |main() -> int
        |    var p: Point
        |    p.x = 3
        |    p.y = 4
        |    dist_sq(p)
        |""".stripMargin)
  }
