package jjm.datasets.conll08

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

import jjm.ling.PredArgStructure
import jjm.datasets.PropBankPredicate

import monocle.macros.Lenses

case class CoNLL08SentenceId(
  split: CoNLL08Split, // train, dev, test
  index: Int // sentence index
) {
  override def toString = s"$split:$index"
}
object CoNLL08SentenceId {
  private def isInt(s: String) = scala.util.Try(s.toInt).toOption.nonEmpty
  def fromString(x: String): Option[CoNLL08SentenceId] = x.split(":").toList match {
    case CoNLL08Split(split) :: index :: Nil if isInt(index) =>
      Some(CoNLL08SentenceId(split, index.toInt))
    case _ => None
  }

  // implicit val conll08SentenceIdEncoder: Encoder[CoNLL08SentenceId] =
  //   Encoder.encodeString.contramap[CoNLL08SentenceId](_.toString)
  // implicit val conll08SentenceIdDecoder: Decoder[CoNLL08SentenceId] =
  //   Decoder.decodeString.emap(
  //     x => fromString(x).fold[Either[String, CoNLL08SentenceId]](
  //       Left(s"$x is not a valid CoNLL 2008 Sentence ID"))(
  //       Right(_))
  //   )
}

@Lenses case class CoNLL08Sentence(
  id: CoNLL08SentenceId,
  tokens: Vector[CoNLL08Token],
  paddingIndices: Set[Int], // keep track of split word forms
  childToParentDependencies: Vector[(String, Int)],
  predicateArgumentStructures: List[PredArgStructure[PropBankPredicate, Int]])
