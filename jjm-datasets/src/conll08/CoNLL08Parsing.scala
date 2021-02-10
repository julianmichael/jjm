package jjm.datasets.conll08

// import jjm.ling.ESpan
// import jjm.implicits._

import cats.data.NonEmptyList
import cats.implicits._

import jjm.ling.PredArgStructure
import jjm.datasets.PropBankPredicate

object CoNLL08Parsing {

  /** Reads a CoNLL08Sentence from a list of lines taken from the CoNLL2008 dataset.
    *
    * Does not expect empty lines on either end of the list.
    * Assumes the lines are taken from a CoNLL 2008 data file,
    * undefined behavior otherwise.
    */
  def readSentence(id: CoNLL08SentenceId, lines: NonEmptyList[String]): CoNLL08Sentence = {
    val rows = lines.map(_.split("\t").toVector).toList.toVector
    val columns = rows.transpose
    val tokens = rows.map(f =>
      CoNLL08Token(
        index = f(0).toInt - 1,
        pos = Option(f(3)).filter(_ != '_').getOrElse(f(7)),
        token = f(5),
        lemma = f(6)
      )
    )

    val paddingIndices = columns(1).zipWithIndex
      .filter(_._1 == "_")
      .map(_._2).toSet

    val dependencies = rows.map(f =>
      f(9) -> (f(8).toInt - 1)
    )

    val predicates = rows.filter(f => f(10) != "_").map { f =>
      val lemma :: sense :: Nil = f(10).split("\\.").toList
      val index = f(0).toInt - 1
      index -> PropBankPredicate(lemma = lemma, sense = sense)
    }
    val argLists = columns.drop(11).map(_.zipWithIndex.filter(_._1 != "_").toList)
    require(predicates.size == argLists.size)
    val paStructures = predicates.zip(argLists).map { case ((predIndex, pred), args) =>
      PredArgStructure(predIndex, pred, args)
    }.toList

    CoNLL08Sentence(id, tokens, paddingIndices, dependencies, paStructures)
  }

  /** Reads a CoNLL 2008 file as an iterator over lines. */
  def readFile(split: CoNLL08Split, lines: Iterator[String]): Vector[CoNLL08Sentence] = {
    val (sentences, extraLines, _) = lines.foldLeft((List.empty[CoNLL08Sentence], List.empty[String], 0)) {
      case (acc @ (prevSentences, curLines, sentenceNum), line) =>
        if(line.trim.isEmpty) NonEmptyList.fromList(curLines).fold(acc) { neCurLines =>
          (readSentence(CoNLL08SentenceId(split, sentenceNum), neCurLines.reverse) :: prevSentences, Nil, sentenceNum + 1)
        } else {
          (prevSentences, line :: curLines, sentenceNum)
        }
    }
    require(extraLines.isEmpty)
    sentences.toVector.reverse
  }


  import fs2.Pipe

  // TODO replace with .toNel after fs2 update
  /** Reads a CoNLL 2008 file as a stream of lines; returns a stream of sentences */
  def streamSentencesFromLines[F[_]](split: CoNLL08Split): Pipe[F, String, CoNLL08Sentence] = lines => {
    lines.groupAdjacentBy(_.trim.nonEmpty)
      .collect { case (isNonEmpty, sentenceLines) if isNonEmpty => NonEmptyList.fromList(sentenceLines.toList).get }
      .zipWithIndex
      .map { case (linesNel, sentenceNum) => readSentence(CoNLL08SentenceId(split, sentenceNum.toInt), linesNel) }
  }
}

