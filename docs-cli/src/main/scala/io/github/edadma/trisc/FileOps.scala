package io.github.edadma.trisc

trait FileOps:
  def readFile(path: String): String
  def writeFile(path: String, content: String): Unit
  def exists(path: String): Boolean
  def isDirectory(path: String): Boolean
  def listFiles(path: String): Seq[String] // full paths
  def fileName(path: String): String       // basename
  def mkdirs(path: String): Unit
  def joinPath(dir: String, name: String): String

  /** Resolve to an absolute filesystem path. Used by the dep resolver to
   *  record path-dep locations in `sysl.lock` so the lock content does not
   *  depend on the cwd from which the CLI was invoked. */
  def absolutePath(path: String): String

object FileOps:
  // Set by platform-specific entry point before CLI runs
  var instance: FileOps = null
