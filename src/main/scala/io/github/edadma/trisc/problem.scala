package io.github.edadma.trisc

import scala.util.parsing.input.Position

def problem(pos: Position, msg: String): Nothing =
  val error =
    if pos == null then msg
    else if pos.line == 1 then s"$msg\n${pos.longString}"
    else s"${pos.line}: $msg\n${pos.longString}"

  sys.error(error)
