package io.github.edadma.trisc

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("process", JSImport.Namespace)
private object Process extends js.Object:
  val argv: js.Array[String] = js.native

object Main:
  def main(args: Array[String]): Unit =
    FileOps.instance = JsFileOps
    val userArgs = Process.argv.toSeq.drop(2)
    DocsCli.parse(userArgs) match
      case Some(config) => DocsCli.execute(config)
      case None         => // scopt already printed usage/error
