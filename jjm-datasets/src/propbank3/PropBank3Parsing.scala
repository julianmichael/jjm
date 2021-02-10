package jjm.datasets.propbank3

import scala.util.Try

import cats.implicits._

import jjm.ling.PredArgStructure
import jjm.datasets.PropBankPredicate
import jjm.datasets.propbank1.PropBankArgument
import jjm.datasets.propbank1.PropBank1Parsing

object PropBank3Parsing {

  import PropBank1Parsing._

  def readPredArgStructure(line: String): PredArgStructure[PropBankPredicate, PropBankArgument] = {
    val fields = line.split(" ").toVector
    val predicateIndex = fields(2).toInt
    // require(fields(3) == "gold") // not always gold. sometimes is some version marker. idk
    // val predLemma = fields(4) // not using for now.
    val predFields = fields(5).split("\\.")
    val predicate = PropBankPredicate(
      lemma = predFields(0),
      sense = predFields(1))
    // fields(6) is "aspects" it seems, doc'd as "no longer used" in latest release
    // I wonder why. guess aspect is hard
    val arguments = fields.drop(7).map(readPropBankArgument(line, _))

    PredArgStructure(predicateIndex, predicate, arguments.toList)
  }

  import fs2.Pipe

  def streamPredArgStructuresFromProps[F[_]]: Pipe[
    F, String, (PropBank3SentenceId, PredArgStructure[PropBankPredicate, PropBankArgument])
  ] = lines => {
    lines.filter(_.trim.nonEmpty).map { line =>
      val pathStr :: sentenceNumStr :: Nil = line.split(" ").toList.take(2)
      val sentenceId = PropBank3SentenceId.parse(pathStr, sentenceNumStr.toInt)
      sentenceId -> readPredArgStructure(line)
    }
  }

  def streamSentencesFromCoNLLSkels[F[_]]: Pipe[F, String, PropBank3SentenceCoNLLStyle] = lines => {
    lines.filter(_.trim.nonEmpty)
      .groupAdjacentBy(_.split("\\s+").toList.take(2))
      .map { case (prefix, lines) =>
        val pathStr :: sentenceNumStr :: Nil = prefix
        val sentenceId = PropBank3SentenceId.parse(
          pathStr.replaceAll("ontonotes/", ""), // remove this prefix which is here for some reason
          sentenceNumStr.toInt
        )

        val lineArrays = lines.toVector.map(_.split("\\s+"))
        val predicates = for {
          (arr, index) <- lineArrays.zipWithIndex.toList
          predicateLemma = arr(6)
          if !predicateLemma.equals("-")
          sense = arr(7)
          if !sense.equals("-")
          // predicatePos = arr(4)
          // head = words(index)
        } yield index -> PropBankPredicate(lemma = predicateLemma, sense = sense)

        val paStructures = for {
          ((predIndex, pred), num) <- predicates.zipWithIndex // num of predicate tells us which col the args are in
          spansString = lineArrays.map(arr => arr(8 + num)).combineAll
          argSpans = jjm.datasets.ontonotes5.PredicateArgumentStructureParsing.readArgumentSpans(spansString)
        } yield PredArgStructure(predIndex, pred, argSpans)

        PropBank3SentenceCoNLLStyle(
          sentenceId,
          paStructures
        )
      }
  }
}
