package jjm.datasets.propbank1

import scala.util.Try

import cats.implicits._

import jjm.ling.PredArgStructure
import jjm.datasets.PropBankPredicate
import jjm.ling.SyntaxTree
import jjm.datasets.ptb2.PTB2FilePath
import jjm.datasets.ptb2.PTB2SentenceId

object PropBank1Parsing {

  def readSyntaxTreeBranch(branchString: String): SyntaxTree.Branch = {
    val parts = branchString.split(":")
    val beginIndex = parts(0).toInt
    val constituentHeight = parts(1).toInt
    SyntaxTree.Branch(beginIndex, constituentHeight)
  }

  def readArgumentSpec(argSpec: String): PropBankArgument = {
    // linking seems to happen last, i.e., with loosest associativity/binding
    if(argSpec.contains("*")) {
      PropBankArgument.Linked(argSpec.split("\\*").map(readArgumentSpec).toList)
    } else if(argSpec.contains(";")) {
      PropBankArgument.ICHConcat(argSpec.split(";").map(readArgumentSpec).toList)
    } else if(argSpec.contains(",")) {
      PropBankArgument.Concat(argSpec.split(",").map(readArgumentSpec).toList)
    } else PropBankArgument.Node(readSyntaxTreeBranch(argSpec))
  }

  def readPropBankArgument(line: String, argString: String): (String, PropBankArgument) = {
    val label = argString.dropWhile(_ != '-').tail
    val spansString = argString.takeWhile(_ != '-')
    // if(
    //   List(
    //     spansString.contains("*"),
    //     spansString.contains(";"),
    //     spansString.contains(","),
    //     ).filter(identity).size > 1
    // ) {
    //   System.err.println("AAAAAHH!!")
    //   System.err.println(line)
    //   System.err.println(label)
    //   System.err.println(spansString)
    // }

    label -> readArgumentSpec(spansString)
  }

  def readPredArgStructure(line: String): PredArgStructure[PropBankPredicate, PropBankArgument] = {
    val fields = line.split(" ").toVector
    val predicateIndex = fields(2).toInt
    require(fields(3) == "gold")
    val predFields = fields(4).split("\\.")
    val predicate = PropBankPredicate(
      lemma = predFields(0),
      sense = predFields(1))
    // fields(5) is "aspects" it seems, doc'd as "no longer used" in latest release
    // I wonder why. guess aspect is hard
    val arguments = fields.drop(6).map(readPropBankArgument(line, _))

    PredArgStructure(predicateIndex, predicate, arguments.toList)
  }

  import fs2.Pipe

  def streamSentencesFromLines[F[_]]: Pipe[F, String, PropBank1Sentence] = lines => {
    lines.filter(_.trim.nonEmpty)
      .groupAdjacentBy(_.split(" ").toList.take(2))
      .map { case (prefix, lines) =>
        val pathStr :: sentenceNumStr :: Nil = prefix
        val path = {
          val pathParts = pathStr.split("/").tail
          PTB2FilePath(pathParts(0).toInt, pathParts(1))
        }
        val sentenceNum = sentenceNumStr.toInt
        val sentenceId = PTB2SentenceId(path, sentenceNum)
        PropBank1Sentence(
          sentenceId,
          lines.toList.map(readPredArgStructure)
        )
      }
  }
}
