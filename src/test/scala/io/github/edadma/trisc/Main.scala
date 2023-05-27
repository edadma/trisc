package io.github.edadma.trisc

import pprint.pprintln

@main def run(): Unit =
  val r = AssemblyParser.parseExpr("3.4e-5")

  pprintln(r)
