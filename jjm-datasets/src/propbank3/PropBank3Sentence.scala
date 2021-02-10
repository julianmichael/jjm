package jjm.datasets.propbank3

import jjm.ling.PredArgStructure
import jjm.datasets.PropBankPredicate
import jjm.datasets.propbank1.PropBankArgument

case class PropBank3Sentence(
  id: PropBank3SentenceId,
  predArgStructures: List[PredArgStructure[PropBankPredicate, PropBankArgument]]
)

import jjm.ling.ESpan

case class PropBank3SentenceCoNLLStyle(
  id: PropBank3SentenceId,
  predArgStructures: List[PredArgStructure[PropBankPredicate, ESpan]]
)
