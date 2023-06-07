package io.github.edadma.trisc

class Emulator(memory: Seq[(String, String, Long)]) /*extends Addressable*/:
  def load(tof: TOF): Unit = {}

  def run(): Unit = {}

  def step(): Unit = {}

  def registers: (Long, Long) = null

  def disassemble: String = null
