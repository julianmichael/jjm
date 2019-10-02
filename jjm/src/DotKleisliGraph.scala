package jjm

// import jjm.implicits._

import cats.Applicative
import cats.Eq
import cats.~>
import cats.Monad
import cats.Monoid
import cats.MonoidK
import cats.Semigroup
import cats.implicits._

import io.circe.Encoder
import io.circe.Decoder
import io.circe.KeyEncoder
import io.circe.KeyDecoder
import io.circe.HCursor
import io.circe.Json

import monocle.Traversal
import monocle.function.At
import monocle.function.Index

// TODO maybe use a different name for the F[_] version and DotKleisliGraph for F = Id?
// inner map always contains an explicit entry for every element of the key type
class DotKleisliGraph[F[_], A <: Dot] private (private val map: Map[A, F[_]]) { self =>

  def apply(key: A): F[key.Out] =
    map(key).asInstanceOf[F[key.Out]]

  def update(key: A)(value: F[key.Out]): DotKleisliGraph[F, A] =
    new DotKleisliGraph[F, A](map + (key -> value))

  def sequence[G[_] : Monad, H[_]](
    separate: F ~> Lambda[B => G[H[B]]]
  ): G[DotKleisliGraph[H, A]] = {
    map.toList.foldM[G, Map[A, H[_]]](Map.empty[A, H[_]]) { case (m, (k, v)) =>
      separate(v).map(hv => m + (k -> hv))
    }.map(new DotKleisliGraph[H, A](_))
  }

  // def keySet: Set[A] =
  //   map.keySet

  // def iterator: Iterator[DotPair[F, A]] = map.iterator.map { case (k, v) =>
  //   DotPair[F](k)(v.asInstanceOf[F[k.Out]])
  // }

  // def size: Int =
  //   map.size

  override def toString =
    map.toString

  override def equals(that: Any): Boolean = that match {
    case that: DotKleisliGraph[_, _] => this.map == that.map
    case _ => false
  }

  override def hashCode: Int = map.hashCode

  def toDotKleisli: DotKleisli[λ[B => F[B]], A] =
    new DotKleisli[λ[B => F[B]], A] {
      def apply(a: A) = self.apply(a)
    }

  def toFunctionK: DotFunctionK[F, A] =
    new DotFunctionK[F, A] {
      override def apply[B](fa: A { type Out = B }): F[B] =
        self.apply(fa)
    }
}

object DotKleisliGraph extends DotKleisliGraphInstances {

  def fromDotKleisli[F[_], A <: Dot : Finite](f: DotKleisli[F, A]) = {
    new DotKleisliGraph(
      Finite[A].values.foldLeft(Map.empty[A, F[_]])(
        (map, key) => map + (key -> f(key))
      )
    )
  }

  def fromFunctionK[F[_], A <: Dot : Finite](f: DotFunctionK[F, A]) = {
    fromDotKleisli(DotKleisli.fromFunctionK(f))
  }

}

abstract private[jjm] class DotKleisliGraphInstances extends DotKleisliGraphInstances0 {

  // TODO probably should only be done with underlying Map based on proper Hash and Eq classes
  // implicit def dotMapEq[A <: Dot](
  //   implicit eq: Eq[A],
  //   dotEq: DotKleisli[Eq, A]
  // ) = new Eq[DotKleisliGraph[A]] {
  //   override def eqv(x: DotKleisliGraph[F, G], y: DotKleisliGraph[F, G]): Boolean =
  //     x.map == y.map
  // }

  // TODO: semigroupK, commutative monoid/semigroup, and anything else from Cats

  implicit def dotKleisliGraphMonoid[F[_], A <: Dot : Finite](
    implicit dotMonoid: DotKleisli[λ[B => Monoid[F[B]]], A]
  ): Monoid[DotKleisliGraph[F, A]] =
    new Monoid[DotKleisliGraph[F, A]] {
      override def empty =
        DotKleisliGraph.fromDotKleisli(
          dotMonoid.andThenK(
            λ[λ[B => Monoid[F[B]]] ~> F](_.empty)
          )
        )
      override def combine(x: DotKleisliGraph[F, A], y: DotKleisliGraph[F, A]) = {
        DotKleisliGraph.fromFunctionK(
          new DotFunctionK[F, A] {
            def apply[B](key: A { type Out = B }): F[B] =
              dotMonoid(key).combine(x(key), y(key))
          }
        )
      }
    }

  implicit def dotKleisliGraphEncoder[F[_], A <: Dot : Finite](
    implicit keyEncoder: KeyEncoder[A],
    dotEncoder: DotKleisli[λ[B => Encoder[F[B]]], A]
  ): Encoder[DotKleisliGraph[F, A]] = new Encoder[DotKleisliGraph[F, A]] {
    final def apply(m: DotKleisliGraph[F, A]) = Json.obj(
      Finite[A].values.toList.map(key =>
        keyEncoder(key) -> dotEncoder(key)(m(key))
      ): _*
    )
  }

  implicit def dotKleisliGraphDecoder[F[_], A <: Dot : Finite](
    implicit keyEncoder: KeyEncoder[A],
    dotDecoder: DotKleisli[λ[B => Decoder[F[B]]], A]
  ): Decoder[DotKleisliGraph[F, A]] = new Decoder[DotKleisliGraph[F, A]] {
    final def apply(c: HCursor): Decoder.Result[DotKleisliGraph[F, A]] = {
      DotKleisliGraph.fromFunctionK[Lambda[B => Decoder.Result[F[B]]], A](
        new DotFunctionK[Lambda[B => Decoder.Result[F[B]]], A] {
          def apply[B](key: A { type Out = B }): Decoder.Result[F[B]] =
            dotDecoder(key).tryDecode(c.downField(keyEncoder(key)))
        }
      ).sequence[Decoder.Result, F](
        Lambda[Lambda[B => Decoder.Result[F[B]]] ~> Lambda[B => Decoder.Result[F[B]]]](x => x)
      )
    }
  }

  // TODO Traversal over DotPair elements, if we can get DotPair to work...

  implicit def dotKleisliGraphAt[F[_], A <: Dot, Out0]: At[DotKleisliGraph[F, A], A { type Out = Out0 }, F[Out0]] =
    At[DotKleisliGraph[F, A], A { type Out = Out0 }, F[Out0]](
      i => map => map(i))(
      i => v => map => map.update(i)(v)
    )
}

sealed trait DotKleisliGraphInstances0 extends DotKleisliGraphInstances1 {
  implicit def dotKleisliGraphMonoidFromMonoidK[F[_]: MonoidK, A <: Dot : Finite]: Monoid[DotKleisliGraph[F, A]] =
    new Monoid[DotKleisliGraph[F, A]] {
      override def empty = DotKleisliGraph.fromFunctionK[F, A](
        new DotFunctionK[F, A] {
          def apply[B](a: A { type Out = B }): F[B] = MonoidK[F].empty[B]
        }
      )
      override def combine(x: DotKleisliGraph[F, A], y: DotKleisliGraph[F, A]) = {
        DotKleisliGraph.fromFunctionK[F, A](
          new DotFunctionK[F, A] {
            def apply[B](a: A { type Out = B }): F[B] = MonoidK[F].combineK(x(a), y(a))
          }
        )
      }
    }
}

sealed trait DotKleisliGraphInstances1 {
  implicit def dotKleisliGraphSemigroup[F[_], A <: Dot : Finite](
    implicit dotSemigroup: DotKleisli[λ[B => Semigroup[F[B]]], A]
  ): Semigroup[DotKleisliGraph[F, A]] =
    new Semigroup[DotKleisliGraph[F, A]] {
      override def combine(x: DotKleisliGraph[F, A], y: DotKleisliGraph[F, A]) = {
        DotKleisliGraph.fromFunctionK[F, A](
          new DotFunctionK[F, A] {
            def apply[B](a: A { type Out = B }): F[B] = dotSemigroup(a).combine(x(a), y(a))
          }
        )
      }
    }
}
