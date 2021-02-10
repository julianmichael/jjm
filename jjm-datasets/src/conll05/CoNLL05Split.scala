package jjm.datasets.conll05

import cats.Order
import cats.implicits._

// import io.circe.{Encoder, Decoder}
// import io.circe.generic.JsonCodec

sealed trait CoNLL05Split {
  import CoNLL05Split.{Train, Dev, TestWSJ, TestBrown}
  override def toString = this match {
    case Train => "train"
    case Dev => "devel"
    case TestWSJ => "test.wsj"
    case TestBrown => "test.brown"
  }

  def isTest = this match {
    case TestWSJ => true
    case TestBrown => true
    case _ => false
  }
}
object CoNLL05Split {
  case object Train extends CoNLL05Split
  case object Dev extends CoNLL05Split
  case object TestWSJ extends CoNLL05Split
  case object TestBrown extends CoNLL05Split

  val all = Set[CoNLL05Split](Train, Dev, TestWSJ, TestBrown)

  def unapply(splitStr: String): Option[CoNLL05Split] =
    fromString(splitStr)

  def fromString(x: String): Option[CoNLL05Split] = x match {
    case "train" => Some(Train)
    case "devel" => Some(Dev)
    case "test.wsj" => Some(TestWSJ)
    case "test.brown" => Some(TestBrown)
    case _ => None
  }

  val conll05SplitOrder: Order[CoNLL05Split] = Order.by[CoNLL05Split, Int] {
    case Train => 0
    case Dev => 1
    case TestWSJ => 2
    case TestBrown => 3
  }
}
