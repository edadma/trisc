package io.github.edadma.trisc

import scala.util.parsing.input.Positional

def problem(elem: Positional, msg: String): Nothing =
  val error =
    if elem == null || elem.pos == null then msg
    else if elem.pos.line == 1 then s"$msg\n${elem.pos.longString}"
    else s"${elem.pos.line}: $msg\n${elem.pos.longString}"

  sys.error(error)

def warning(elem: Positional, msg: String): Unit =
  val error =
    if elem == null || elem.pos == null then msg
    else if elem.pos.line == 1 then s"$msg\n${elem.pos.longString}"
    else s"${elem.pos.line}: $msg\n${elem.pos.longString}"

  println(error)
