package jjm.ling

import simulacrum._
import scala.language.implicitConversions

/*
 * Typeclass for types that can be rendered as a list of Penn-Treebank style tokens.
 */
@typeclass trait HasAlignedTokens[-A] extends HasTokens[A] {
  /* Returns a vector of Penn Treebank style tokens. */
  @op("alignedTokens") def getAlignedTokens(a: A): Vector[AlignedToken]

  override def getTokens(a: A): Vector[String] =
    getAlignedTokens(a).map(_.token)
}

object HasAlignedTokens {
  object strictOps extends ToHasAlignedTokensOps
}
