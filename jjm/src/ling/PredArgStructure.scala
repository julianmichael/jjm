package jjm.ling

import io.circe.generic.JsonCodec

import monocle.macros.GenLens

@JsonCodec case class PredArgStructure[Pred, Arg](
  predicateIndex: Int,
  predicate: Pred,
  arguments: List[(String, Arg)])

object PredArgStructure {
  def predicateIndex[Pred, Arg] = GenLens[PredArgStructure[Pred, Arg]](_.predicateIndex)
  def predicate[Pred, Arg] = GenLens[PredArgStructure[Pred, Arg]](_.predicate)
  def arguments[Pred, Arg] = GenLens[PredArgStructure[Pred, Arg]](_.arguments)
}
