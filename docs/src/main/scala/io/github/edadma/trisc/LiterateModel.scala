package io.github.edadma.trisc

sealed trait LiterateBlock:
  def startLine: Int

case class CodeBlock(content: String, startLine: Int) extends LiterateBlock
case class ProseBlock(tag: String, format: Option[String], content: String, startLine: Int) extends LiterateBlock
case class ApiBlock(content: String, startLine: Int) extends LiterateBlock

case class LiterateDocument(blocks: List[LiterateBlock], sourceLineMap: Map[Int, Int])

case class ChannelConfig(name: String, includeTags: Set[String])

object ChannelConfig:
  val code: ChannelConfig = ChannelConfig("code", Set("code"))
  val dev: ChannelConfig = ChannelConfig("dev", Set("code", "tech", "tech-latex", "main"))
  val website: ChannelConfig = ChannelConfig("website", Set("main", "main-latex"))
  val api: ChannelConfig = ChannelConfig("api", Set("api"))
  val reference: ChannelConfig = ChannelConfig("reference", Set("code", "api"))

  val defaults: Map[String, ChannelConfig] = Map(
    "code" -> code,
    "dev" -> dev,
    "website" -> website,
    "api" -> api,
    "reference" -> reference,
  )
