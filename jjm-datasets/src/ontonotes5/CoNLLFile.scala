package jjm.datasets.ontonotes5

import io.circe.generic.JsonCodec

import jjm.ling.ESpan
import jjm.ling.SyntaxTree
import jjm.ling.PredArgStructure

import jjm.datasets.PropBankPredicate

/** Represents a single CoNLL annotation file.
  *
  * @param id the unique ID of the file, present on its first line
  * @param sentences all of the sentences in the annotation file
  */
@JsonCodec case class CoNLLFile(
  path: CoNLLPath,
  sentences: Vector[CoNLLSentence]
)

/** Represents an annotated sentence from the CoNLL data.
  *
  * As of now, we're lazy-loading the members of this class,
  * only implementing them as we need them.
  * I believe coref spans are already implemented in the coref annotation project;
  * if necessary ask me (Julian) and I can put them in.
  */
@JsonCodec case class CoNLLSentence(
  path: CoNLLSentencePath,
  partNum: Int,
  tokens: List[CoNLLToken],
  syntaxTree: SyntaxTree[CoNLLToken],
  predicateArgumentStructures: List[PredArgStructure[PropBankPredicate, ESpan]]
    // nerSpans: Nothing, // TODO
    // corefSpans: List[CorefSpan] // TODO
) {
  def sentenceNum = path.sentenceNum
}
object CoNLLSentence

@JsonCodec case class CoNLLPath(
  split: String, // development, train
  language: String, // arabic, chinese, english
  domain: String, // depends on language; e.g., nw, bc, wb
  source: String, // e.g., wsj, xinhua
  section: Int, // always falls within 0-99
  name: String, // filename prefix, usually equal to source
  number: Int // always falls within 0-99
) {
  def documentId = f"$domain%s/$source%s/$section%02d/$name%s_$section%02d$number%02d"
  def suffix = s"data/$split/data/$language/annotations/$documentId.gold_conll"
}

object CoNLLPath {
  private[this] val pathSuffixRegex =
    """data/(.*?)/data/(.*?)/annotations/(.*?)/(.*?)/([0-9]{2})/(.*?)_[0-9]{2}([0-9]{2}).gold_conll""".r
  private[this] object IntMatch {
    def unapply(s: String): Option[Int] = scala.util.Try(s.toInt).toOption
  }
  def fromPathSuffix(s: String): Option[CoNLLPath] = s match {
    case pathSuffixRegex(split, language, domain, source, IntMatch(section), name, IntMatch(number)) =>
      Some(CoNLLPath(split, language, domain, source, section, name, number))
    case _ => None
  }
}

/** Represents a unique index to a CoNLL sentence.
  *
  * This can be used to easily serialize a sentence without worrying about the data definition changing.
  * The FileManager extension methods for the conll package include one to retrieve a sentence directly
  * from such a path.
  *
  * @param filePath the path to the CoNLL file containing this sentence
  * @param sentenceNum the index of this sentence in the document
  */
@JsonCodec case class CoNLLSentencePath(
  filePath: CoNLLPath,
  sentenceNum: Int
) {
  override def toString = s"${filePath.suffix}:$sentenceNum"
}
object CoNLLSentencePath {
  def fromString(s: String): Option[CoNLLSentencePath] = {
    val entries = s.split(":")
    require(entries.length == 2)
    CoNLLPath.fromPathSuffix(entries(0)).flatMap(filePath =>
      scala.util.Try(entries(1).toInt).toOption.map(sentenceNum =>
        CoNLLSentencePath(filePath, sentenceNum)
      )
    )
  }
}
