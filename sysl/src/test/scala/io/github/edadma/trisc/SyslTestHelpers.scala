package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait SyslTestHelpers extends AnyFreeSpec with Matchers {

  def run(source: String): (Long, String) =
    val buf = new StringBuilder
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter(s => buf ++= s)
    val result = interp.run(typed)
    (result, buf.toString)

  def eval(source: String): Long = run(source)._1

  def output(source: String): String = run(source)._2
}
