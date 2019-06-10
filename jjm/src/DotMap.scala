package jjm

import cats.Applicative
import cats.Eq
import cats.~>
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

// TODO maybe use a different name for the F[_] version and DotMap for F = Id?
final class DotMap[F[_], A <: Dot] private (private val map: Map[A, F[_]]) {

  def get(key: A): Option[F[key.Out]] =
    map.get(key).asInstanceOf[Option[F[key.Out]]]

  def put(key: A)(value: F[key.Out]): DotMap[F, A] =
    new DotMap[F, A](map + (key -> value))

  def +(pair: DotPair[F, A]): DotMap[F, A] =
    new DotMap[F, A](map + (pair.fst -> pair.snd))

  def remove(key: A): DotMap[F, A] =
    new DotMap[F, A](map - key)

  def keys: Iterable[A] =
    map.keys

  def keySet: Set[A] =
    map.keySet

  def iterator: Iterator[DotPair[F, A]] = map.iterator.map { case (k, v) =>
    DotPair[F](k)(v.asInstanceOf[F[k.Out]])
  }

  def size: Int =
    map.size

  override def toString =
    map.toString

  override def equals(that: Any): Boolean = that match {
    case that: DotMap[_, _] => this.map == that.map
    case _ => false
  }

  override def hashCode: Int = map.hashCode

  def toDotKleisli: DotKleisli[λ[B => Option[F[B]]], A] =
    new DotKleisli[λ[B => Option[F[B]]], A] {
      def apply(a: A) = get(a)
    }

  def toFunctionK: (DotF[A]#Aux ~> λ[B => Option[F[B]]]) =
    new (DotF[A]#Aux ~> λ[B => Option[F[B]]]) {
      override def apply[B](fa: DotF[A]#Aux[B]): Option[F[B]] =
        get(fa)
    }
}

object DotMap extends DotMapInstances {
  def empty[F[_], A <: Dot] = new DotMap[F, A](Map.empty[A, F[A#Out]])

  def apply[F[_], A <: Dot](pairs: DotPair[F, A]*): DotMap[F, A] = {
    pairs.foldLeft(empty[F, A])(_ + _)
  }
}

abstract private[jjm] class DotMapInstances extends DotMapInstances0 {

  // TODO probably should only be done with underlying Map based on proper Hash and Eq classes
  // implicit def dotMapEq[A <: Dot](
  //   implicit eq: Eq[A],
  //   dotEq: DotKleisli[Eq, A]
  // ) = new Eq[DotMap[A]] {
  //   override def eqv(x: DotMap[F, G], y: DotMap[F, G]): Boolean =
  //     x.map == y.map
  // }

  // TODO: commutative monoid/semigroup, and anything else from Cats...

  implicit def dotMapMonoid[F[_], A <: Dot](
    implicit dotMonoid: DotKleisli[λ[B => Monoid[F[B]]], A]
  ): Monoid[DotMap[F, A]] =
    new Monoid[DotMap[F, A]] {
      override def empty =
        DotMap.empty[F, A]
      override def combine(x: DotMap[F, A], y: DotMap[F, A]) = {
        (x.keys ++ y.keys).toSet.foldLeft(empty) {
          case (dMap, key) => dMap.put(key) {
            implicit val m = dotMonoid(key)
            (x.get(key).combineAll |+| y.get(key).combineAll)
          }
        }
      }
    }

  implicit def dotMapEncoder[F[_], A <: Dot](
    implicit keyEncoder: KeyEncoder[A],
    dotEncoder: DotKleisli[λ[B => Encoder[F[B]]], A]
  ): Encoder[DotMap[F, A]] = new Encoder[DotMap[F, A]] {
    final def apply(m: DotMap[F, A]) = Json.obj(
      // TODO would be best if I can iterate over DotPairs but I can't seem to get those to work
      m.keys.map(key =>
        keyEncoder(key) -> dotEncoder(key)(m.get(key).get)
      ).toSeq: _*
    )
  }

  implicit def dotMapDecoder[F[_], A <: Dot](
    implicit keyDecoder: KeyDecoder[A],
    dotDecoder: DotKleisli[λ[B => Decoder[F[B]]], A]
  ): Decoder[DotMap[F, A]] = new Decoder[DotMap[F, A]] {
    final def apply(c: HCursor): Decoder.Result[DotMap[F, A]] = {
      // TODO aah replace the get
      c.keys.get.toList.foldM[Decoder.Result, DotMap[F, A]](DotMap.empty[F, A]) { (m, keyStr) =>
        val key = keyDecoder(keyStr).get // TODO aah replace the get
        val value = dotDecoder(key).tryDecode(c.downField(keyStr))
        // val foo = Foo(key)
        // type Out = foo.Out
        value.map(v => m.put(key)(v))
      }
    }
  }

  // TODO Traversal over DotPair elements, if we can get DotPair to work...

  implicit def dotMapAt[F[_], A <: Dot, Out0]: At[DotMap[F, A], A { type Out = Out0 }, Option[F[Out0]]] =
    At[DotMap[F, A], A { type Out = Out0 }, Option[F[Out0]]](
      i => map => map.get(i))(
      i => optV => map => optV.fold(map.remove(i))(v => map.put(i)(v))
    )

  implicit def dotMapIndex[F[_], A <: Dot, Out0]: Index[DotMap[F, A], A { type Out = Out0 }, F[Out0]] = Index.fromAt

  // NOTE: it would be nice to have something like UnorderedFoldable, but we can't since DotMap is not a proper functor
  // intuitively we could instead use a Traversal,
  // but we can't do a Traversal over values because their types depend on their particular keys.
  // We also shouldn't do a Traversal over pairs because modifications could end up collapsing the structure non-deterministically.
  // but I'm not sure if we actually want to be able to do modifications
  // over pairs since that may collapse the structure.
  // -- Here we have that not-great definition of Traversal. I don't think we want it. Just convert to a List instead.
  // implicit def dotMapTraversal[F[_], A <: Dot]: Traversal[DotMap[F, A], DotPair[F, A]] = new Traversal[DotMap[F, A], DotPair[F, A]] {
  //   def modifyF[G[_]: Applicative](f: DotPair[F, A] => G[DotPair[F, A]])(s: DotMap[F, A]): G[DotMap[F, A]] = {
  //     s.iterator.toList.traverse(f).map(
  //       _.foldLeft(DotMap.empty[F, A])(_ + _)
  //     )
  //   }
  // }
}

sealed trait DotMapInstances0 extends DotMapInstances1 {
  implicit def dotMapMonoidFromMonoidK[F[_]: MonoidK, A <: Dot]: Monoid[DotMap[F, A]] =
    new Monoid[DotMap[F, A]] {
      override def empty = DotMap.empty[F, A]
      override def combine(x: DotMap[F, A], y: DotMap[F, A]) = {
        (x.keys ++ y.keys).toSet.foldLeft(DotMap.empty[F, A]) {
          case (dMap, key) => dMap.put(key) {
            (x.get(key).foldK <+> y.get(key).foldK)
          }
        }
      }
    }
}

sealed trait DotMapInstances1 {
  implicit def dotMapSemigroup[F[_], A <: Dot](
    implicit dotSemigroup: DotKleisli[λ[B => Semigroup[F[B]]], A]
  ): Semigroup[DotMap[F, A]] =
    new Semigroup[DotMap[F, A]] {
      override def combine(x: DotMap[F, A], y: DotMap[F, A]) = {
        (x.keys ++ y.keys).toSet.foldLeft(DotMap.empty[F, A]) {
          case (dMap, key) => dMap.put(key) {
            implicit val m = dotSemigroup(key)
            (x.get(key), y.get(key)) match {
              case (Some(xv), Some(yv)) => xv |+| yv
              case (Some(xv), None) => xv
              case (None, Some(yv)) => yv
              case (None, None) => ??? // should never happen
            }
          }
        }
      }
    }
}
