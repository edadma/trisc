package io.github.edadma.trisc

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("fs", JSImport.Namespace)
private object FS extends js.Object:
  def readFileSync(path: String, encoding: String): String = js.native
  def writeFileSync(path: String, data: String): Unit = js.native
  def existsSync(path: String): Boolean = js.native
  def statSync(path: String): js.Dynamic = js.native
  def readdirSync(path: String): js.Array[String] = js.native
  def mkdirSync(path: String, options: js.Dynamic): Unit = js.native

@js.native
@JSImport("path", JSImport.Namespace)
private object Path extends js.Object:
  def basename(path: String): String = js.native
  def join(paths: String*): String = js.native

object JsFileOps extends FileOps:
  def readFile(path: String): String = FS.readFileSync(path, "utf8")
  def writeFile(path: String, content: String): Unit = FS.writeFileSync(path, content)
  def exists(path: String): Boolean = FS.existsSync(path)
  def isDirectory(path: String): Boolean = FS.statSync(path).isDirectory().asInstanceOf[Boolean]
  def listFiles(path: String): Seq[String] = FS.readdirSync(path).toSeq.map(name => Path.join(path, name))
  def fileName(path: String): String = Path.basename(path)
  def mkdirs(path: String): Unit = FS.mkdirSync(path, js.Dynamic.literal(recursive = true))
  def joinPath(dir: String, name: String): String = Path.join(dir, name)
