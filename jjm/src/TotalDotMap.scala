package jjm

import cats.Id
import cats.~>
import cats.Monoid
import cats.MonoidK
import cats.Semigroup
import cats.implicits._

import monocle.function.At

// TODO maybe use a different name for the F[_] version and TotalDotMap for F = Id?
class TotalDotMap[F[_], A <: Dot] private[jjm] (private val map: Map[A, F[_]]) { self =>

  def apply(key: A): F[key.Out] =
    map(key).asInstanceOf[F[key.Out]]

  def update(key: A)(value: F[key.Out]): TotalDotMap[F, A] =
    new TotalDotMap[F, A](map + (key -> value))

  override def toString =
    map.toString

  override def equals(that: Any): Boolean = that match {
    case that: TotalDotMap[_, _] => this.map == that.map
    case _ => false
  }

  override def hashCode: Int = map.hashCode

  def toDotKleisli: DotKleisli[F, A] =
    new DotKleisli[F, A] {
      def apply(a: A) = self.apply(a)
    }

  def toFunctionK: DotFunctionK[F, A] =
    new DotFunctionK[F, A] {
      override def apply[B](fa: A { type Out = B }): F[B] =
        self.apply(fa)
    }

  def toDotKleisliGraph(implicit fin: Finite[A]) =
    DotKleisliGraph.fromDotKleisli(this.toDotKleisli)

  // access to underlying map for more efficient implementations of typeclass instances

  // private[jjm] def populatedKeys: Iterable[A] =
  //   map.keys

  private[jjm] def populatedKeySet: Set[A] =
    map.keySet

  // private[jjm] def populatedIterator: Iterator[DotPair[F, A]] = map.iterator.map { case (k, v) =>
  //   DotPair[F](k)(v.asInstanceOf[F[k.Out]])
  // }

  // private[jjm] def populatedSize: Int =
  //   map.size

}

object TotalDotMap extends TotalDotMapInstances {
  def fromDotKleisli[F[_], A <: Dot](f: DotKleisli[F, A]) =
    new TotalDotMap[F, A](Map.empty[A, F[A#Out]].withDefault(f.apply(_)))

  def fromFunctionK[F[_], A <: Dot](f: DotFunctionK[F, A]) =
    fromDotKleisli(DotKleisli.fromFunctionK(f))

  def fromDotFunction[A <: Dot](f: DotFunction[A]) =
    new TotalDotMap[Id, A](Map.empty[A, A#Out].withDefault(f.apply(_)))

  def lazyCombine[F[_], A <: Dot](x: TotalDotMap[F, A], y: TotalDotMap[F, A])(
    implicit dotSemigroup: DotKleisli[λ[B => Semigroup[F[B]]], A]
  ): DotKleisli[F, A] = DotKleisli.fromFunctionK[F, A](
    new DotFunctionK[F, A] {
      def apply[B](key: A { type Out = B }): F[B] = {
        implicit val m = dotSemigroup(key)
        x(key) |+| y(key)
      }
    }
  )

}

abstract private[jjm] class TotalDotMapInstances extends TotalDotMapInstances0 {

  // NOTE: We lose a lot of typeclass instances because of the reliance of
  // TotalDotMap on an underlying function (and not knowing all of the
  // inhabitants of the key type), which prevents them from working with
  // reasonable semantics.
  // Instances prevented: Eq, Hash, Encoder, Decoder, ...
  // For something like TotalMap with these semantics, use FinTotalDotMap.

  // TODO: semigroupK, commutative monoid/semigroup, and anything else from Cats

  implicit def totalDotMapMonoid[F[_], A <: Dot](
    implicit dotMonoid: DotKleisli[λ[B => Monoid[F[B]]], A]
  ): Monoid[TotalDotMap[F, A]] =
    new Monoid[TotalDotMap[F, A]] {
      override def empty =
        TotalDotMap.fromDotKleisli(
          dotMonoid.andThenK(
            λ[λ[B => Monoid[F[B]]] ~> F](_.empty)
          )
        )
      override def combine(x: TotalDotMap[F, A], y: TotalDotMap[F, A]) = {
        (x.populatedKeySet ++ y.populatedKeySet).toSet.foldLeft(DotMap.empty[F, A]) {
          case (dMap, key) => dMap.put(key) {
            implicit val m = dotMonoid(key)
            x(key) |+| y(key)
          }
        }.withDefault(
          TotalDotMap.lazyCombine[F, A](x, y)(
            dotMonoid.andThenK[
              λ[B => Semigroup[F[B]]]
            ](λ[λ[B => Monoid[F[B]]] ~> λ[B => Semigroup[F[B]]]](x => x))
          )
        )
      }
    }

  implicit def totalDotMapAt[F[_], A <: Dot, Out0]: At[TotalDotMap[F, A], A { type Out = Out0 }, F[Out0]] =
    At[TotalDotMap[F, A], A { type Out = Out0 }, F[Out0]](
      i => map => map(i))(
      i => v => map => map.update(i)(v)
    )
}

sealed trait TotalDotMapInstances0 extends TotalDotMapInstances1 {
  implicit def dotMapMonoidFromMonoidK[F[_]: MonoidK, A <: Dot]: Monoid[TotalDotMap[F, A]] =
    new Monoid[TotalDotMap[F, A]] {
      override def empty = TotalDotMap.fromFunctionK[F, A](
        new DotFunctionK[F, A] {
          def apply[B](a: A { type Out = B }): F[B] = MonoidK[F].empty[B]
        }
      )
      override def combine(x: TotalDotMap[F, A], y: TotalDotMap[F, A]) = {
        (x.populatedKeySet ++ y.populatedKeySet).toSet.foldLeft(DotMap.empty[F, A]) {
          case (dMap, key) => dMap.put(key) {
            x(key) <+> y(key)
          }
        }.withDefault(
          TotalDotMap.lazyCombine[F, A](x, y)(
            DotKleisli.fromFunctionK[λ[B => Semigroup[F[B]]], A](
              new DotFunctionK[λ[B => Semigroup[F[B]]], A] {
                def apply[B](a: A { type Out = B }): Semigroup[F[B]] =
                  Semigroup.instance(MonoidK[F].combineK[B])
              }
            )
          )
        )
      }
    }
}

sealed trait TotalDotMapInstances1 {
  implicit def dotMapSemigroup[F[_], A <: Dot](
    implicit dotSemigroup: DotKleisli[λ[B => Semigroup[F[B]]], A]
  ): Semigroup[TotalDotMap[F, A]] =
    new Semigroup[TotalDotMap[F, A]] {
      override def combine(x: TotalDotMap[F, A], y: TotalDotMap[F, A]) = {
        (x.populatedKeySet ++ y.populatedKeySet).toSet.foldLeft(DotMap.empty[F, A]) {
          case (dMap, key) => dMap.put(key) {
            implicit val m = dotSemigroup(key)
            x(key) |+| y(key)
          }
        }.withDefault(TotalDotMap.lazyCombine[F, A](x, y))
      }
    }
}
