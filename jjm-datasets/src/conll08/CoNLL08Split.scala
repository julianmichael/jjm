package jjm.datasets.conll08

import cats.Order
import cats.implicits._

sealed trait CoNLL08Split {
  import CoNLL08Split.{Trial, Train, Dev, TestWSJ, TestBrown}
  override def toString = this match {
    case Train => "train"
    case Dev => "devel"
    case TestWSJ => "test.wsj"
    case TestBrown => "test.brown"
    case Trial => "trial"
  }

  def isTest = this match {
    case TestWSJ => true
    case TestBrown => true
    case _ => false
  }
}
object CoNLL08Split {
  case object Train extends CoNLL08Split
  case object Dev extends CoNLL08Split
  case object TestWSJ extends CoNLL08Split
  case object TestBrown extends CoNLL08Split
  case object Trial extends CoNLL08Split

  val all = Set[CoNLL08Split](Train, Dev, TestWSJ, TestBrown, Trial)

  def unapply(splitStr: String): Option[CoNLL08Split] =
    fromString(splitStr)

  def fromString(x: String): Option[CoNLL08Split] = x match {
    case "train" => Some(Train)
    case "devel" => Some(Dev)
    case "test.wsj" => Some(TestWSJ)
    case "test.brown" => Some(TestBrown)
    case "trial" => Some(Trial)
    case _ => None
  }

  val conll08SplitOrder: Order[CoNLL08Split] = Order.by[CoNLL08Split, Int] {
    case Train => 0
    case Dev => 1
    case TestWSJ => 2
    case TestBrown => 3
    case Trial => 4
  }
}
