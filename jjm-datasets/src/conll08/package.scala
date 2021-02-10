package jjm.datasets

import shapeless.{HList, ::, HNil}

/** Utilities for working with the CoNLL 2008 Shared Task data, found here:
  * https://catalog.ldc.upenn.edu/LDC2009T12
  */
package object conll08 {

  import jjm.ling._

  type CoNLL08Token = Index :: Pos :: Token :: Lemma :: HNil
  object CoNLL08Token {
    def apply(index: Int, pos: String, token: String, lemma: String): CoNLL08Token = {
      Index.field(index) :: Pos.field(pos) :: Token.field(token) :: Lemma.field(lemma) :: HNil
    }
  }
}
