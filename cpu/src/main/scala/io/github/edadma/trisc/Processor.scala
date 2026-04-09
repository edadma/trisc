package io.github.edadma.trisc

/** Abstract processor interface for hardware devices.
  *
  * Both TRISC CPU and SVM extend this trait, allowing devices (InterruptController, Timer, etc.) to work with either
  * processor without coupling to a specific implementation.
  */
trait Processor extends Addressable:
  var state: State
  var cycles: Long
  var limit: Int

  /** Signal an external interrupt to the processor. */
  def interrupt(): Unit

  /** Reset the processor and begin execution from the reset vector / entry point. */
  def reset(): Unit

  /** Execute the main run loop until halt, limit, or double fault. */
  def run(): Unit

  /** Resume execution after a halt (e.g., breakpoint). */
  def resume(): Unit
