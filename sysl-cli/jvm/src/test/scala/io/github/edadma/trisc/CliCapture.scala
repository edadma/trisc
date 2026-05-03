package io.github.edadma.trisc

import java.io.{ByteArrayOutputStream, PrintStream}

/** JVM-global lock for test suites that capture stdout/stderr by mutating
 *  `System.out` / `System.err`. Two such suites running in parallel (sbt's
 *  `-P` test parallelism enables it across suites) clobber each other's
 *  captures and produce flaky empty-output failures.
 *
 *  All call sites must go through `CliCapture.runCli` so the lock is held
 *  for the full duration of an SyslCli invocation including capture flush. */
object CliCapture:

  private val ioLock = new Object

  /** Run `sysl <args...>` programmatically, returning the combined captured
   *  stdout + stderr, plus an exit-style code (0 = clean, 1 = SyslCli threw,
   *  2 = scopt parse failure). */
  def runCli(args: Seq[String]): (Int, String) = ioLock.synchronized {
    val baos = new ByteArrayOutputStream()
    val out = new PrintStream(baos, true, "UTF-8")
    val savedSystemOut = System.out
    val savedSystemErr = System.err
    System.setOut(out)
    System.setErr(out)
    val code: Int =
      try Console.withOut(out) {
        Console.withErr(out) {
          SyslCli.parse(args) match
            case Some(config) =>
              try { SyslCli.execute(config); 0 }
              catch
                case e: RuntimeException =>
                  out.println(s"[runtime error] ${e.getClass.getSimpleName}: ${e.getMessage}")
                  1
            case None => 2
        }
      }
      finally
        out.flush()
        System.setOut(savedSystemOut)
        System.setErr(savedSystemErr)
    (code, baos.toString("UTF-8"))
  }
