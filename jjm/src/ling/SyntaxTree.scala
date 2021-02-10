package jjm.ling

import cats.Apply
import cats.Applicative
import cats.Eval
import cats.Monad
import cats.NonEmptyTraverse
import cats.data.NonEmptyList
import cats.implicits._

import io.circe.generic.JsonCodec

import monocle.macros._

import jjm.implicits._

/** Represents a syntax tree. */
@JsonCodec sealed trait SyntaxTree[Word] {
  import SyntaxTree.{Node, Leaf, Branch}
  final def cata[A](leaf: Word => A)(node: (String, NonEmptyList[A]) => A): A = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) => node(label, children.map(_.cata(leaf)(node)))
  }

  final def cataUnlabeled[A](leaf: Word => A)(node: NonEmptyList[A] => A): A = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) => node(children.map(_.cataUnlabeled(leaf)(node)))
  }

  final def cataM[M[_]: Monad, A](leaf: Word => M[A])(node: (String, NonEmptyList[A]) => M[A]): M[A] = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) =>
      children.traverse(_.cataM(leaf)(node)).flatMap(node(label, _))
  }

  final def cataUnlabeledM[M[_]: Monad, A](leaf: Word => M[A])(node: NonEmptyList[A] => M[A]): M[A] = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) =>
      children.traverse(_.cataUnlabeledM(leaf)(node)).flatMap(node)
  }

  final def size = cataUnlabeled(_ => 1)(_.combineAll)
  final def depth = cataUnlabeled(_ => 0)(_.maximum + 1)
  final def beginIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.head)
  final def endIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.last)
  final def toSpan(implicit ev: HasIndex[Word]) = ESpan(beginIndex, endIndex + 1)

  final def getSubtree(branch: Branch)(
    implicit ev: HasIndex[Word]
  ): SyntaxTree[Word] = {
    // System.err.println(s"Getting subtree: ${branch}")
    // System.err.println(s"Tree: ${this}")
    getSubtreeAux(branch, Nil).getOrElse(
      throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
    )
  }

  final def getSubtreeAux(
    branch: Branch, ancestors: List[SyntaxTree[Word]])(
    implicit ev: HasIndex[Word]
  ): Option[SyntaxTree[Word]] = this match {
    case Leaf(token) =>
      if(token.index == branch.tokenIndex) Some {
        (this :: ancestors).lift(branch.constituentHeight).getOrElse(
          throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
        )
      } else None
    case Node(_, children) =>
      children.toList.flatMap(_.getSubtreeAux(branch, this :: ancestors)).headOption
  }

  def getHighestUnaryBranch(p: Word => Boolean): Option[SyntaxTree[Word]] =
    getHighestUnaryBranchAux(p, None)
  def getHighestUnaryBranchAux(
    p: Word => Boolean, topAncestor: Option[SyntaxTree[Word]]
  ): Option[SyntaxTree[Word]] = this match {
    case Leaf(token) => if(p(token)) topAncestor.orElse(Some(Leaf(token))) else None
    case Node(_, children) =>
      // if >1 child, we're no longer valid top of branch.
      // if <= 1 child, our top ancestor remains valid. if there is none, we're it.
      val ancestor = if(children.size > 1) None else topAncestor.orElse(Some(this))
      children.toList.flatMap(_.getHighestUnaryBranchAux(p, ancestor)).headOption
  }

  // levels: non-empty list of unexplored siblings. includes `this` as head of first one.
  // @annotation.tailrec
  // final def getSubtreeAux(
  //   branch: Branch, levels: NonEmptyList[NonEmptyList[SyntaxTree[Word]]])(
  //   implicit ev: HasIndex[Word]
  // ): SyntaxTree[Word] = {
  //   System.err.println(s"$this\n.getSubtreeAux($branch, $levels)")
  //   this match {
  //     case Leaf(token) =>
  //       if(token.index == branch.tokenIndex) {
  //         levels.get(branch.constituentHeight).fold(
  //           throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
  //         )(_.head)
  //       } else levels.head.tail match {
  //         // not done with this set of siblings yet
  //         case next :: remSiblings =>
  //           next.getSubtreeAux(branch, NonEmptyList(NonEmptyList(next, remSiblings), levels.tail))
  //         // done with this set of children, move up in the tree
  //         case Nil => levels.tail match {
  //           case nextLevel :: remLevels =>
  //             NonEmptyList.fromList(nextLevel.tail) match {
  //               // move on to parent's next sibling
  //               case Some(parentRemainingSiblings) =>
  //                 parentRemainingSiblings.head.getSubtreeAux(branch, NonEmptyList(parentRemainingSiblings, remLevels))
  //               // or parent's parent's next sibling... etc.?
  //               case None => NonEmptyList.fromList(remLevels) match {
  //                 case Some(neRemLevels) =>
  //                   neRemLevels.head.head.getSubtreeAux(branch, neRemLevels)
  //                 case None =>
  //                   throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
  //               }
  //             }
  //             // nextLevel.head.getSubtreeAux(branch, NonEmptyList(nextLevel, remLevels))
  //           case Nil =>
  //             throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
  //         }
  //       }
  //     case Node(_, children) => children.head.getSubtreeAux(branch, NonEmptyList(children, levels.toList))
  //   }
  // }

  final def toStringMultiline(renderWord: Word => String) =
    cata(renderWord) { case (nodeLabel, subtreeStrings) =>
      val childrenStr = subtreeStrings.map(_.replaceAll("\n", "\n ")).toList.mkString("\n")
      s"$nodeLabel\n$childrenStr"
    }

  // final def toStringMultiline(renderWord: Word => String): String = toStringMultilineAux(0, renderWord)
  // // TODO could do this with state monad lol
  // protected[structure] def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String

  final def toVector = cataUnlabeled(Vector(_))(_.toList.toVector.flatten)
}

object SyntaxTree {
  /** Represents a nonterminal node of a SyntaxTree.
    *
    * @param label the nonterminal symbol of this node
    * @param this node's children
    */
  @JsonCodec case class Node[Word](
    label: String,
    children: NonEmptyList[SyntaxTree[Word]]
  ) extends SyntaxTree[Word] {
    // override def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String = {
    //   val indent = " " * indentLevel
    //   val childrenStr = children.map(_.toStringMultilineAux(indentLevel + 1)).mkString("\n")
    //   s"$indent$label\n$childrenStr"
    // }
  }

  /** Represents a terminal node of a SyntaxTree.
    *
    * @param word the word at this node
    */
  @JsonCodec case class Leaf[Word](
    word: Word
  ) extends SyntaxTree[Word] {
    // override def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String = {
    //   val indent = " " * indentLevel
    //   val wordStr = renderWord(word)
    //   s"$indent$wordStr"
    // }
  }

  @Lenses case class Branch(
    tokenIndex: Int,
    constituentHeight: Int)

  implicit val syntaxTreeInstances = new NonEmptyTraverse[SyntaxTree] with Monad[SyntaxTree] {
    def nonEmptyTraverse[G[_]: Apply, A, B](fa: SyntaxTree[A])(f: A => G[B]): G[SyntaxTree[B]] = fa match {
      case Leaf(a) => f(a).map(Leaf(_))
      case Node(label, children) => children.nonEmptyTraverse(nonEmptyTraverse(_)(f)).map(Node(label, _))
    }

    def reduceLeftTo[A, B](fa: SyntaxTree[A])(f: A => B)(g: (B, A) => B): B = fa match {
      case Leaf(a) => f(a)
      case Node(label, children) => children.reduceLeftTo(reduceLeftTo(_)(f)(g))((b, c) => foldLeft(c, b)(g))
    }

    def reduceRightTo[A, B](fa: SyntaxTree[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case Leaf(a) => Eval.now(f(a))
      case Node(label, children) => children.reduceRightTo(
        reduceRightTo(_)(f)(g))(
        (c, llb) => llb.map(lb => foldRight(c, lb)(g))
      ).flatten
    }

    def foldLeft[A, B](fa: SyntaxTree[A], b: B)(f: (B, A) => B): B = fa match {
      case Leaf(a) => f(b, a)
      case Node(label, children) => children.foldLeft(b)((b, c) => foldLeft(c, b)(f))
    }

    def foldRight[A, B](fa: SyntaxTree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case Leaf(a) => f(a, lb)
      case Node(label, children) => children.foldRight(lb)(foldRight(_, _)(f))
    }

    def flatMap[A, B](fa: SyntaxTree[A])(f: A => SyntaxTree[B]): SyntaxTree[B] = fa match {
      case Leaf(a) => f(a)
      case Node(label, children) => Node(label, children.map(flatMap(_)(f)))
    }

    // TODO: not stack safe. shouldn't be an issue bc of typical syntax tree size though.
    def tailRecM[A, B](a: A)(f: A => SyntaxTree[Either[A, B]]): SyntaxTree[B] = {
      flatMap(f(a)) {
        case Right(b) => pure(b)
        case Left(nextA) => tailRecM(nextA)(f)
      }
    }

    // @annotation.tailrec
    // def tailRecM[A, B](a: A)(f: A => SyntaxTree[Either[A, B]]): SyntaxTree[B] = f(a) match {
    //   case Leaf(Left(a)) => tailRefM(a)(f)
    //   case Leaf(Right(b)) => Leaf(b)
    //   case Node(label, children) =>
    // }

    def pure[A](x: A): SyntaxTree[A] = Leaf(x)
  }
}
