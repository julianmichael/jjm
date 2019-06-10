package jjm.instances

import jjm.LowerCaseString

import cats.Show
import cats.Order
import cats.Monoid

import io.circe.Encoder
import io.circe.Decoder

trait LowerCaseStringInstances {

  implicit val lowerCaseStringShow: Show[LowerCaseString] = {
    LowerCaseString.lowerCaseStringShow
  }

  implicit val lowerCaseStringMonoid: Monoid[LowerCaseString] = {
    LowerCaseString.lowerCaseStringMonoid
  }

  implicit val lowerCaseStringOrder: Order[LowerCaseString] = {
    LowerCaseString.lowerCaseStringOrder
  }

  implicit val lowerCaseStringEncoder: Encoder[LowerCaseString] = {
    LowerCaseString.lowerCaseStringEncoder
  }

  implicit val lowerCaseStringDecoder: Decoder[LowerCaseString] = {
    LowerCaseString.lowerCaseStringDecoder
  }
}
