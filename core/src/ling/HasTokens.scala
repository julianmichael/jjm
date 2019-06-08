package jjm.ling

import simulacrum._
import scala.language.implicitConversions

/*
 * Typeclass for types that can be rendered as a list of Penn-Treebank style tokens.
 */
@typeclass trait HasTokens[-A] {
  /* Returns a vector of Penn Treebank style tokens. */
  @op("tokens") def getTokens(a: A): Vector[String]
}
