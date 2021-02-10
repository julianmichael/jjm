package jjm.datasets.propbank1

import jjm.ling.SyntaxTree

import cats.implicits._

import jjm.ling.ESpan
import jjm.ling.{HasIndex, HasToken, HasPos}
import jjm.implicits._

sealed trait PropBankArgument {
  import PropBankArgument._
  def allBranches: List[SyntaxTree.Branch] = this match {
    case Node(branch) => List(branch)
    case compound: CompoundPropBankArgument => compound.subArgs.flatMap(_.allBranches)
  }
  def getTopConstituentLabels[A: HasIndex : HasToken : HasPos](tree: SyntaxTree[A]): List[String] = {
    val allConstituents = allBranches.map { branch =>
      if(branch.constituentHeight == 0) {
        val parent = tree.getSubtree(branch.copy(constituentHeight = 1))
        if(parent.size == 1) parent
        else tree.getSubtree(branch)
      } else tree.getSubtree(branch)
    }

    val topConstituents = allConstituents.filter {
      case SyntaxTree.Node(TracedNonterminal(label, index), _) =>
        val indexMarker = s"*-$index"
        // only keep if no other constituent contains a trace of this one.
        !allConstituents.exists(_.toList.exists(_.token.endsWith(indexMarker)))
      case _ => true
    }

    def stripTraceMarkers(x: String) = x match {
      case TracedNonterminal(y, _) => y
      case _ => x
    }

    def getTraces(t: SyntaxTree[A]) = t.cata[Map[Int, Map[String, Int]]](
        leaf = _ => Map[Int, Map[String, Int]]())(
        node = (label, childIndices) => {
          val nodeIndices = label match {
            case TracedNonterminal(label, indexStr) => Map(indexStr.toInt -> Map(label -> 1))
            case _ => Map[Int, Map[String, Int]]()
          }
          nodeIndices |+| childIndices.combineAll
        }
      )
    val linkedTraces = allConstituents.foldMap(getTraces)
    // val allTraces = getTraces(tree)

    topConstituents.map {
      case SyntaxTree.Leaf(word) => word.pos
      case SyntaxTree.Node(rawLabel, children) =>
        val label = stripTraceMarkers(rawLabel)

        label

        // children.head match {
        //   case SyntaxTree.Leaf(_) => label
        //   case SyntaxTree.Node(potentialSbjLabel, potentialSbjChildren) =>
        //     val fstToken = potentialSbjChildren.head.toNonEmptyList.head
        //     val indexIsCovered = fstToken.token match {
        //       case TraceMarker(label, indexStr) => linkedTraces.keySet.contains(indexStr.toInt)
        //       case _ => topConstituents.exists {
        //         case SyntaxTree.Node("NP-SBJ", _) => true
        //         case _ => false
        //       }
        //     }

        //     if(
        //       potentialSbjLabel.startsWith("NP-SBJ")
        //         && fstToken.pos == "-NONE-"
        //         && !indexIsCovered
        //         && label == "S"
        //         && children.tail.headOption.exists {
        //           case SyntaxTree.Node("VP", _) => true
        //           case _ => false
        //         }
        //     ) "VP" else label
        // }

        // val traces = children.foldMap(_.toList).flatMap { token =>
        //   if(token.pos == "-NONE-") {
        //     val indexOpt = token.token match {
        //       case TraceMarker(label, indexStr) => Some(indexStr.toInt)
        //       case _ => None
        //     }
        //     val unaryBranch = tree.getHighestUnaryBranch(_ == token).get
        //     val label = unaryBranch.cata(_.pos)((label, _) => label)
        //     Some(indexOpt -> Map(label -> 1))
        //   } else None
        // }.toMap
        // // val unfilledTraces = traces.filterNot(p => linkedTraces.keySet.contains(p._1))

        // val gapSymbols = traces.toList.flatMap {
        //   case (None, counts) => Some(counts)
        //   case (Some(index), counts) if !linkedTraces.keySet.contains(index) => Some(counts)
        //   case _ => None
        // }.unorderedFold.toList
        //   .sortBy(_._1).flatMap { case (label, count) =>
        //     Vector.fill(count)(label)
        //   }.mkString(",")

        // if(gapSymbols.nonEmpty) {
        //   label + s"(-$gapSymbols)"
        // } else label
    }
  }
}
sealed trait CompoundPropBankArgument extends PropBankArgument {
  def subArgs: List[PropBankArgument]
}
object PropBankArgument {
  case class Node(branch: SyntaxTree.Branch) extends PropBankArgument
  case class Linked(val subArgs: List[PropBankArgument]) extends CompoundPropBankArgument
  case class ICHConcat(val subArgs: List[PropBankArgument]) extends CompoundPropBankArgument
  case class Concat(val subArgs: List[PropBankArgument]) extends CompoundPropBankArgument

  val TracedNonterminal = "(.*)-([0-9])+".r
  val TraceMarker = "(.*\\*)-([0-9])+".r
}
