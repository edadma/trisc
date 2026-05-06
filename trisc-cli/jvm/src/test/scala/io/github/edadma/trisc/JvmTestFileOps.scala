package io.github.edadma.trisc

import java.io.{File, PrintWriter}

/** Shared FileOps for JVM test drivers. Enables transitive import
  * resolution from the filesystem so tests don't need to manually
  * list every dependency. */
object JvmTestFileOps extends FileOps:
  def readFile(path: String): String =
    val source = scala.io.Source.fromFile(path)
    try source.mkString finally source.close()
  def writeFile(path: String, content: String): Unit =
    val writer = new PrintWriter(path)
    try writer.write(content) finally writer.close()
  def exists(path: String): Boolean = new File(path).exists()
  def isDirectory(path: String): Boolean = new File(path).isDirectory
  def listFiles(path: String): Seq[String] = new File(path).listFiles().toSeq.map(_.getPath)
  def fileName(path: String): String = new File(path).getName
  def mkdirs(path: String): Unit = new File(path).mkdirs()
  def joinPath(dir: String, name: String): String = new File(dir, name).getPath
  def absolutePath(path: String): String = new File(path).getAbsoluteFile.getPath
