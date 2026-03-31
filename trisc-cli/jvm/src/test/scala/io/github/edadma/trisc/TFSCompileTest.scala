package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TFSCompileTest extends AnyFreeSpec with Matchers {

  "TFS module compiles to assembly" in {
    val raw = scala.io.Source.fromFile("tos/tfs/tfs.lsysl").mkString
    val doc = new LiterateParser().parse(raw)
    val tfsSource = LiterateRenderer.tangle(doc)
    val driver = new SyslDriver
    val result = driver.compile(Map("tfs" -> tfsSource))
    val codegen = new SyslTriscCodegen
    for unit <- result.units do
      val asm = codegen.generate(unit.typed)
      // Print first lines with "ldi" or "sti" that might have large immediates
      asm.linesIterator.zipWithIndex.foreach { (line, idx) =>
        if line.contains("ldi") || line.contains("sti") then
          val trimmed = line.trim
          // Check for numeric immediates
          val parts = trimmed.split(",").map(_.trim)
          if parts.length >= 2 then
            val last = parts.last
            try
              val n = if last.startsWith("0x") then java.lang.Long.parseLong(last.substring(2), 16)
                      else if last.startsWith("-") then last.toLong
                      else if last.forall(_.isDigit) then last.toLong
                      else -999
              if n != -999 && (n < -128 || n > 255) then
                println(s"LINE $idx: $trimmed  [VALUE=$n OUT OF BYTE RANGE]")
            catch case _: Exception => ()
      }
      try
        val tof = assemble(asm, relocatable = true)
        println("Assembly succeeded!")
      catch
        case e: Exception =>
          println(s"Assembly FAILED: ${e.getMessage}")
          // Print lines around the error
          asm.linesIterator.zipWithIndex.take(20).foreach { (line, idx) =>
            println(f"$idx%4d: $line")
          }
          fail(e.getMessage)
  }
}
