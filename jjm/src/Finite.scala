package jjm

import simulacrum._
import scala.language.implicitConversions

// Finitely inhabited type.
// not committing to an order.
// not necessarily bounded bc the type might be empty.
@typeclass trait Finite[A] {
  def values: Set[A]
  def numValues: Int = values.size
}
