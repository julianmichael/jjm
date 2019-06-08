package jjm

import scala.language.implicitConversions

import cats.Show
import cats.Order
import cats.Monoid

import io.circe.Encoder
import io.circe.Decoder

sealed trait LowerCaseStringCapsule {
  type Type
  protected type LowerCaseString = Type

  trait StringOps {
    def lowerCase: LowerCaseString
  }

  trait LowerCaseStringOps {
    def +(s: LowerCaseString): LowerCaseString
    def contains(s: LowerCaseString): Boolean
    def startsWith(s: LowerCaseString): Boolean
    def endsWith(s: LowerCaseString): Boolean
    def substring(beginIndex: Int): LowerCaseString
    def substring(beginIndex: Int, endIndex: Int): LowerCaseString
  }

  def fromString(s: String): LowerCaseString
  def toString(lcs: LowerCaseString): String

  def getLowerCaseStringOps(lcs: LowerCaseString): LowerCaseStringOps
  def getStringOps(s: String): StringOps

  def lowerCaseStringShow: Show[LowerCaseString]
  def lowerCaseStringMonoid: Monoid[LowerCaseString]
  def lowerCaseStringOrder: Order[LowerCaseString]
  def lowerCaseStringEncoder: Encoder[LowerCaseString]
  def lowerCaseStringDecoder: Decoder[LowerCaseString]
}

protected[jjm] object LowerCaseStringImpl extends LowerCaseStringCapsule {
  override type Type = String

  class StringOpsImpl(val s: String) extends StringOps { // TODO AnyVal
    override def lowerCase: LowerCaseString = s.toLowerCase
  }

  class LowerCaseStringOpsImpl(val s1: LowerCaseString) extends LowerCaseStringOps { // TODO AnyVal
    override def +(s2: LowerCaseString) = s1 + s2
    override def contains(s2: LowerCaseString) = s1 contains s2
    override def startsWith(s2: LowerCaseString) = s1 startsWith s2
    override def endsWith(s2: LowerCaseString) = s1 endsWith s2
    override def substring(beginIndex: Int) = s1.substring(beginIndex)
    override def substring(beginIndex: Int, endIndex: Int) = s1.substring(beginIndex, endIndex)
  }

  override def fromString(s: String): LowerCaseString = s.toLowerCase
  override def toString(s: LowerCaseString): String = s

  override def getLowerCaseStringOps(s: LowerCaseString) = new LowerCaseStringOpsImpl(s)
  override def getStringOps(s: String) = new StringOpsImpl(s)

  override val lowerCaseStringShow: Show[LowerCaseString] =
    new Show[LowerCaseString] {
      override def show(lcs: LowerCaseString): String = lcs.toString
    }

  override val lowerCaseStringMonoid: Monoid[LowerCaseString] =
    new Monoid[LowerCaseString] {
      def empty: LowerCaseString = ""

      def combine(x: LowerCaseString, y: LowerCaseString): LowerCaseString =
        x + y

      override def combineAll(xs: TraversableOnce[LowerCaseString]): LowerCaseString = {
        val sb = new StringBuilder
        xs.foreach(sb.append)
        sb.toString
      }
    }

  override val lowerCaseStringOrder: Order[LowerCaseString] =
    new Order[LowerCaseString] {

      override def eqv(x: String, y: String): Boolean = x == y

      override def compare(x: String, y: String): Int =
        if (x eq y) 0 else x compareTo y
    }

  override val lowerCaseStringEncoder: Encoder[LowerCaseString] = Encoder.encodeString
  override val lowerCaseStringDecoder: Decoder[LowerCaseString] = Decoder.decodeString.map(_.toLowerCase)
}




