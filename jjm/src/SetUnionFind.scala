package jjm

import cats.Order

import scala.collection.immutable.Set

import io.circe.generic.JsonCodec

/** Basic, inefficient immutable union-find based on sets.
  * Invariant: all sets are disjoint.
  * TODO: write a custom json codec that enforces this?
  */
@JsonCodec case class SetUnionFind[A](
  val sets: Set[Set[A]]
) {

  def remove(a: A): SetUnionFind[A] = {
    SetUnionFind(sets.map(_.filter(_ != a)).filter(_.nonEmpty))
  }

  def add(a: A): SetUnionFind[A] =
    if(sets.forall(!_.contains(a))) SetUnionFind(sets + Set[A](a))
    else this

  def find(a: A) = for {
    subset <- sets.find(_.contains(a))
  } yield subset.head

  /** Unions a and b, adding them in if they're not already present */
  def union(a: A, b: A): SetUnionFind[A] = {
    val aSet = sets.find(_.contains(a)).getOrElse(Set(a))
    val bSet = sets.find(_.contains(b)).getOrElse(Set(b))
    SetUnionFind(sets - aSet - bSet + (aSet ++ bSet))
  }

  /** Unions a and b only if they're already present */
  def unionStrict(a: A, b: A): Option[SetUnionFind[A]] = for {
    aSet <- sets.find(_.contains(a))
    bSet <- sets.find(_.contains(b))
  } yield if(aSet != bSet) {
    SetUnionFind(sets - aSet - bSet + (aSet ++ bSet))
  } else this

  def representatives: Set[A] = sets.map(_.head)
}
object SetUnionFind {
  def empty[A]: SetUnionFind[A] = SetUnionFind(Set[Set[A]]())
}
