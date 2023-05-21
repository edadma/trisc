package io.github.edadma.trisc

import scala.collection.immutable

class CPU:
  val r = immutable.ArraySeq(
    new Reg0,
    new Reg,
    new Reg,
    new Reg,
    new Reg,
    new Reg,
    new Reg,
    new Reg,
  )

  var status: Int = 0

  class Reg:
    private var r: Int = 0

    def read: Int = r

    def write(v: Int): Unit = r = v

  class Reg0 extends Reg:
    override def read: Int = 0

    override def write(v: Int): Unit = {}
