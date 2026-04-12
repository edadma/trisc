package io.github.edadma.trisc

import org.scalatest.Tag

/** Tag for slow tests — typically full-OS integration tests that take >5s each.
  *
  * Run without slow tests (fast feedback loop):
  *   sbt "triscCliJVM/testOnly -- -l io.github.edadma.trisc.Slow"
  *
  * Run only slow tests:
  *   sbt "triscCliJVM/testOnly -- -n io.github.edadma.trisc.Slow"
  */
object Slow extends Tag("io.github.edadma.trisc.Slow")
