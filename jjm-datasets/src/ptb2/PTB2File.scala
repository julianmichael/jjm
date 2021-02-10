package jjm.datasets.ptb2

import jjm.ling.SyntaxTree

import io.circe.generic.JsonCodec

@JsonCodec case class PTB2FilePath(
  section: Int,
  fileName: String) { // with .mrg suffix
  def pathSuffix = f"$section%02d/$fileName%s"
  override def toString = pathSuffix
}
object PTB2FilePath {
  def fromString(x: String) = {
    val fields = x.split("/")
    PTB2FilePath(
      section = fields(1).toInt,
      fileName = fields(2)
    )
  }
}

@JsonCodec case class PTB2SentenceId(filePath: PTB2FilePath, sentenceNum: Int) {
  override def toString = s"$filePath:$sentenceNum"
}

case class PTB2File(
  path: PTB2FilePath,
  sentences: Vector[PTB2Sentence])

case class PTB2Sentence(
  id: PTB2SentenceId,
  tokens: Vector[PTB2Token],
  syntaxTree: SyntaxTree[PTB2Token])
