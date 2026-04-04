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

  /** Read a .sysl file from disk. */
  def readSysl(path: String): String =
    scala.io.Source.fromFile(path).mkString

  /** Compile library sources with a test main via the driver, then interpret. Returns exit code. */
  def evalWithLibs(libs: Map[String, String], main: String): Long =
    val sources = libs + ("test" -> main)
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged)

  /** Compile library sources with a test main via the driver, then interpret. Returns (exit code, output). */
  def runWithLibs(libs: Map[String, String], main: String): (Long, String) =
    val buf = new StringBuilder
    val sources = libs + ("test" -> main)
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter(s => buf ++= s)
    val code = interp.run(merged)
    (code, buf.toString)
}
