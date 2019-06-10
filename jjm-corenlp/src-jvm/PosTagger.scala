package jjm.corenlp

import jjm.ling.HasToken
import jjm.ling.Pos
import jjm.implicits._

import cats.Foldable
import cats.implicits._

import shapeless.HList
import shapeless.ops.record.Updater
import shapeless.record._

import edu.stanford.nlp.tagger.maxent.MaxentTagger

// TODO: let the cache expire or something...
object PosTagger {

  lazy val tagger: MaxentTagger = new MaxentTagger(
    "edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger"
  );

  val posTagCache = collection.mutable.Map.empty[Vector[String], Vector[String]]

  /** POS-tags a sequence of tokens. */
  def posTag[F[_]: Foldable, A <: HList : HasToken : Updater[?, Pos]](s: F[A]) = {
    val recVec = s.toList.toVector
    val origTokens = recVec.map(_.token)
    // probably we can do that with a tagger parameter...but...how about later..
    val posTags = posTagCache.get(origTokens) match {
      case None =>
        val result = tagger
          .tagTokenizedString(origTokens.mkString(" "))
          .split(" ")
          .toVector
          .map(_.split("_"))
          .map(_(1))
        posTagCache.put(origTokens, result)
        result
      case Some(result) => result
    }
    recVec.zip(posTags).map { case (tok, pos) => tok.updated(Pos, pos) }
  }
}
