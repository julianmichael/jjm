package jjm.ling.en

// utilities related to Penn Treebank pos tags.
// add more here as needed
object PTBPosTags {
  val nouns = Set("NN", "NNS", "NNP", "NNPS")
  val adverbs = Set("RB", "RBR", "RBS", "WRB") // not sure if we really want WRB. oh well
  val plurals = Set("NNS", "NNPS")
  val verbs = Set("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")
  val whs = Set("WDT", "WP", "WP$", "WRB")
  val adjectives = Set("JJ", "JJR", "JJS")

  val symbols = Set(
    ".",
    "$",
    "-LRB-",
    "-LCB-",
    "-LSB-",
    "-RRB-",
    "-RCB-",
    "-RSB-"
  ) // there may be others.

  val all = Set(
    "CC",
    "CD",
    "DT",
    "EX",
    "FW",
    "IN", // then adjectives
    "LS",
    "MD", // then nouns
    "PDT",
    "POS",
    "PRP",
    "PRP$", // then adverbs
    "RP",
    "SYM",
    "TO",
    "UH" // then verbs, then whs
  ) ++ adjectives ++ nouns ++ adverbs ++ verbs ++ whs ++ symbols
}
