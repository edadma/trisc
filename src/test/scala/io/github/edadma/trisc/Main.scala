package io.github.edadma.trisc

import pprint.pprintln

@main def run(): Unit =
  val r = AssemblyParser.parseExpr(""" asdf """)

  pprintln(r)
