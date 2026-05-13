package io.github.edadma.trisc

import org.scalatest.Tag

/** Sibling of `triscCliJVM`'s `Tags.scala`. Uses the same tag name so the
  * global `-l io.github.edadma.trisc.Slow` exclusion (in `build.sbt`) skips
  * both subprojects' slow tests by default, and the `testSlow` command's
  * `-n io.github.edadma.trisc.Slow` includes both.
  *
  * Kept separate from `triscCli`'s Tags object because `syslCli` doesn't
  * depend on `triscCli` — ScalaTest matches tags by the string name, not by
  * Scala class identity, so two `Tag("io.github.edadma.trisc.Slow")` objects
  * in different packages share filtering behavior.
  */
object Slow extends Tag("io.github.edadma.trisc.Slow")
