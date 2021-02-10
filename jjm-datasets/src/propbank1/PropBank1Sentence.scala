package jjm.datasets.propbank1

import jjm.ling.PredArgStructure
import jjm.datasets.PropBankPredicate
import jjm.datasets.ptb2.PTB2SentenceId

case class PropBank1Sentence(
  id: PTB2SentenceId,
  predArgStructures: List[PredArgStructure[PropBankPredicate, PropBankArgument]]
)
