package jjm.datasets.propbank3

case class PropBank3SentenceId(
  domain: String, // depends on language; e.g., nw, bc, wb
  source: String, // e.g., wsj, xinhua
  section: Int, // always falls within 0-99
  name: String, // filename prefix, usually equal to source
  number: Int, // always falls within 0-99
  sentenceNum: Int
) {
  override def toString = f"$domain%s/$source%s/$section%s/$name%s_$section%02d$number%02d:$sentenceNum%d"
}
object PropBank3SentenceId {
  def parse(pathStr: String, sentenceNum: Int): PropBank3SentenceId = {
    val pathParts = pathStr.split("/")
    val fileNameNoExt = pathParts(3).takeWhile(_ != '.')
    val fileNameParts = fileNameNoExt.split("_")
    PropBank3SentenceId(
      domain = pathParts(0),
      source = pathParts(1),
      section = pathParts(2).toInt,
      name = fileNameParts(0),
      number = fileNameParts(1).drop(2).toInt,
      sentenceNum = sentenceNum
    )
  }
}
