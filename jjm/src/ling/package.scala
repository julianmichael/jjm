package jjm

import shapeless.{HList, HNil}
import shapeless.labelled.FieldType
import shapeless.ops.record.Selector
import shapeless.record._
import shapeless.syntax.singleton._

import simulacrum._
import scala.language.implicitConversions

package object ling {

  type Token = FieldType[Token.type, String]
  object Token {
    def apply(x: String) = Token ->> x :: HNil
    def field(x: String) = Token ->> x
  }
  @typeclass trait HasToken[T] {
    def token(t: T): String
  }
  object HasToken {
    implicit def recordHasToken[T <: HList : Selector.Aux[*, Token.type, String]] = new HasToken[T] {
      def token(t: T): String = t(Token)
    }
    implicit val tokenHasToken = new HasToken[Token] {
      def token(t: Token): String = t
    }
  }

  type SourceText = FieldType[SourceText.type, TokenText]
  object SourceText {
    def apply(x: TokenText) = SourceText ->> x :: HNil
    def field(x: TokenText) = SourceText ->> x
  }
  @typeclass trait HasSourceText[T] {
    def sourceText(t: T): TokenText
  }
  object HasSourceText {
    implicit def recordHasSourceText[T <: HList : Selector.Aux[*, SourceText.type, TokenText]] = new HasSourceText[T] {
      def sourceText(t: T): TokenText = t(SourceText)
    }
    implicit val sourceTextHasSourceText = new HasSourceText[SourceText] {
      def sourceText(t: SourceText): TokenText = t
    }
  }

  type Index = FieldType[Index.type, Int]
  object Index {
    def apply(x: Int) = Index ->> x :: HNil
    def field(x: Int) = Index ->> x
  }
  @typeclass trait HasIndex[T] {
    def index(t: T): Int
  }
  object HasIndex {
    implicit def recordHasIndex[T <: HList : Selector.Aux[*, Index.type, Int]] = new HasIndex[T] {
      def index(t: T): Int = t(Index)
    }
    implicit val indexHasIndex = new HasIndex[Index] {
      def index(t: Index): Int = t
    }
  }

  type Pos = FieldType[Pos.type, String]
  object Pos {
    def apply(x: String) = Pos ->> x :: HNil
    def field(x: String) = Pos ->> x
  }
  @typeclass trait HasPos[T] {
    def pos(t: T): String
  }
  object HasPos {
    implicit def recordHasPos[T <: HList : Selector.Aux[*, Pos.type, String]] = new HasPos[T] {
      def pos(t: T): String = t(Pos)
    }
    implicit val posHasPos = new HasPos[Pos] {
      def pos(t: Pos): String = t
    }
  }

  type Lemma = FieldType[Lemma.type, String]
  object Lemma {
    def apply(x: String) = Lemma ->> x :: HNil
    def field(x: String) = Lemma ->> x
  }
  @typeclass trait HasLemma[T] {
    def lemma(t: T): String
  }
  object HasLemma {
    implicit def recordHasLemma[T <: HList : Selector.Aux[*, Lemma.type, String]] = new HasLemma[T] {
      def lemma(t: T): String = t(Lemma)
    }
    implicit val lemmaHasLemma = new HasLemma[Lemma] {
      def lemma(t: Lemma): String = t
    }
  }

}
