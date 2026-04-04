package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslAllocTests extends AnyFreeSpec with Matchers {

  private val sources = Map(
    "posix/stdlib/alloc" -> scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString,
    "posix/string/string" -> scala.io.Source.fromFile("posix/string/string.sysl").mkString,
    "posix/unistd/unistd" -> scala.io.Source.fromFile("posix/unistd/unistd.sysl").mkString,
  )

  "allocator parses and analyzes" in {
    val driver = new SyslDriver
    val result = driver.compile(sources)
    result.units.length shouldBe 3
  }
}
