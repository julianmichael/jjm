package jjm.datasets

import shapeless.{HList, ::, HNil}
import shapeless.labelled.FieldType
import shapeless.ops.record.Selector
import shapeless.record._
import shapeless.syntax.singleton._

import simulacrum._
import scala.language.implicitConversions

/** Utilities for the Penn Treebank 2, available at
  * https://catalog.ldc.upenn.edu/LDC95T7
  */
package object ptb2 {

  import jjm.ling._

  type PTB2Token = Index :: Pos :: Token :: HNil
  object PTB2Token {
    def apply(index: Int, pos: String, token: String): PTB2Token = {
      Index.field(index) :: Pos.field(pos) :: Token.field(token) :: HNil
    }
  }
}
