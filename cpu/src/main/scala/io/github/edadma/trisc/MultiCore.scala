package io.github.edadma.trisc

import java.util.concurrent.{CountDownLatch, CyclicBarrier}

/**
 * Shared reservation monitor for cross-core LL/SC invalidation.
 *
 * Each core tracks its own reservation (address + valid flag). When any core
 * writes to memory, all other cores' reservations at that address are invalidated.
 * This ensures LL/SC atomicity across cores.
 *
 * CAS is inherently atomic (single instruction) — the monitor provides
 * a synchronized CAS helper that prevents concurrent CAS on the same address.
 *
 * @param numCores Number of cores in the system
 */
class ReservationMonitor(numCores: Int):
  private val reservationAddr = new Array[Long](numCores)
  private val reservationValid = new Array[Boolean](numCores)

  /** Set a reservation for a core (called by LL). */
  def setReservation(coreId: Int, addr: Long): Unit = synchronized {
    reservationAddr(coreId) = addr
    reservationValid(coreId) = true
  }

  /** Check and clear a reservation for a core (called by SC).
    * Returns true if the reservation was valid and matches the address. */
  def checkAndClear(coreId: Int, addr: Long): Boolean = synchronized {
    val valid = reservationValid(coreId) && reservationAddr(coreId) == addr
    reservationValid(coreId) = false
    valid
  }

  /** Invalidate all other cores' reservations at this address (called on any store). */
  def invalidateOthers(coreId: Int, addr: Long): Unit = synchronized {
    var i = 0
    while i < numCores do
      if i != coreId && reservationValid(i) && reservationAddr(i) == addr then
        reservationValid(i) = false
      i += 1
  }

  /** Clear a specific core's reservation (called on exceptions). */
  def clearReservation(coreId: Int): Unit = synchronized {
    reservationValid(coreId) = false
  }

  /** Execute a CAS atomically across all cores. */
  def atomicCAS(mem: Addressable, addr: Long, expected: Long, newValue: Long): Long = synchronized {
    val old = mem.readLong(addr)
    if old == expected then mem.writeLong(addr, newValue)
    old
  }

/**
 * Multi-core runner. Creates N CPU instances sharing the same memory,
 * each running on its own JVM thread.
 *
 * All cores share:
 *   - Memory (Addressable)
 *   - ReservationMonitor (for LL/SC cross-core invalidation)
 *   - InterruptController (already synchronized)
 *
 * Each core has its own:
 *   - Register file, PC, PSR, USP
 *   - MMU instance (separate page table base per core)
 *   - Tick sequence (can differ per core — e.g., only core 0 gets the timer)
 *
 * Usage:
 * {{{
 *   val mc = new MultiCore(mem, numCores = 4)
 *   mc.core(0).tick = Seq(timer, intc)  // core 0 handles timer
 *   mc.core(1).tick = Seq(intc)          // core 1 only handles interrupts
 *   mc.resetAll()
 *   mc.runAll()  // blocks until all cores halt
 * }}}
 *
 * @param mem      Shared memory
 * @param numCores Number of cores (default 4)
 * @param coreFactory Function to create each CPU given (mem, coreId, monitor)
 */
class MultiCore(
    mem: Addressable,
    val numCores: Int = 4,
    coreFactory: (Addressable, Int, ReservationMonitor) => CPU = null,
):
  val monitor = new ReservationMonitor(numCores)

  private val defaultFactory: (Addressable, Int, ReservationMonitor) => CPU =
    (m, id, mon) => new CPU(m, coreId = id, reservationMonitor = Some(mon))

  private val factory = if coreFactory != null then coreFactory else defaultFactory

  val cores: IndexedSeq[CPU] = (0 until numCores).map(id => factory(mem, id, monitor))

  def core(id: Int): CPU = cores(id)

  /** Reset all cores. */
  def resetAll(): Unit =
    cores.foreach(_.reset())

  /** Run all cores concurrently, blocking until all halt or reach their limit. */
  def runAll(): Unit =
    val latch = new CountDownLatch(numCores)
    val threads = cores.map { cpu =>
      val t = new Thread(() => {
        try cpu.run()
        finally latch.countDown()
      }, s"trisc-core-${cpu.coreId}")
      t.setDaemon(true)
      t
    }
    threads.foreach(_.start())
    latch.await()

  /** Run only the specified cores concurrently. */
  def runCores(ids: Int*): Unit =
    val latch = new CountDownLatch(ids.size)
    val threads = ids.map { id =>
      val cpu = cores(id)
      val t = new Thread(() => {
        try cpu.run()
        finally latch.countDown()
      }, s"trisc-core-$id")
      t.setDaemon(true)
      t
    }
    threads.foreach(_.start())
    latch.await()
