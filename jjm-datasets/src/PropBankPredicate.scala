package jjm.datasets

import io.circe.generic.JsonCodec

@JsonCodec case class PropBankPredicate(
  lemma: String,
  sense: String)
