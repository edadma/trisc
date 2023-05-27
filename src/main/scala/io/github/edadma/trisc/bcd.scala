package io.github.edadma.trisc

import scala.annotation.tailrec

def toBCD(n: Int): Int =
  require(n >= 0, "toBCD: input must be non-negative")

  @tailrec
  def toBCD(n: Int, acc: Int, shift: Int): Int =
    if n == 0 then acc
    else toBCD(n / 10, acc | ((n % 10) << shift), shift + 4)

  toBCD(n, 0, 0)
