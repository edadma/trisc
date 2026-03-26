package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait SyslTestHelpers extends AnyFreeSpec with Matchers {

  def run(source: String): (Long, String) =
    val buf = new StringBuilder
    val Right(program) = (new SyslParser).parseProgram(source): @unchecked
    val interp = new SyslInterpreter(s => buf ++= s)
    val result = interp.run(program)
    (result, buf.toString)

  def eval(source: String): Long = run(source)._1

  def output(source: String): String = run(source)._2
}
