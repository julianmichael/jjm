package jjm

import cats.~>
import cats.Eq
import cats.Monoid
import cats.MonoidK
import cats.implicits._

import io.circe.Encoder
import io.circe.Decoder
import io.circe.KeyEncoder
import io.circe.KeyDecoder
import io.circe.Json
import io.circe.HCursor

import monocle.function.At
import monocle.function.Index

final class DependentMap[F[_], G[_]] private (private val map: Map[F[_], G[_]]) {

  def get[A](key: F[A]): Option[G[A]] =
    map.get(key).asInstanceOf[Option[G[A]]]

  def put[A](key: F[A], value: G[A]): DependentMap[F, G] =
    new DependentMap[F, G](map + (key -> value).asInstanceOf[(F[_], G[_])])

  def put[A](pair: DependentPair[F, G]): DependentMap[F, G] =
    new DependentMap[F, G](map + (pair.fst -> pair.snd).asInstanceOf[(F[_], G[_])])

  def remove[A](key: F[A]): DependentMap[F, G] =
    new DependentMap[F, G](map - key)

  def keys: Iterable[F[_]] =
    map.keys

  def keySet: Set[F[_]] =
    map.keySet

  private[this] def getPair[A0](key: F[A0]): Option[DependentPair[F, G] { type A = A0 }] =
    get(key).map(DependentPair[F, G, A0](key, _))

  def iterator: Iterator[DependentPair[F, G]] =
    map.keys.iterator.map(key => getPair(key).get)

  def size: Int =
    map.size

  override def toString =
    map.toString

  override def equals(that: Any): Boolean = that match {
    case that: DependentMap[_, _] => this.map == that.map
    case _ => false
  }

  override def hashCode: Int = map.hashCode

  def toFunctionK: (F ~> λ[A => Option[G[A]]]) = new (F ~> λ[A => Option[G[A]]]) {
    override def apply[A](fa: F[A]): Option[G[A]] = get(fa)
  }
}

object DependentMap extends DependentMapInstances {
  def empty[F[_], G[_]] =
    new DependentMap[F, G](Map.empty[F[_], G[_]])
}

abstract private[jjm] class DependentMapInstances {

  // TODO should be based on eqs of arg types etc.?
  // probably would only make sense if underlying Map was based on Hash and Eq
  // that's a bit harder to do in this setting bc we basically need EqK, HashK, etc.
  // implicit def dependentMapEq[F[_], G[_]] = new Eq[DependentMap[F, G]] {
  //   override def eqv(x: DependentMap[F, G], y: DependentMap[F, G]): Boolean =
  //     x.map == y.map
  // }

  implicit def dependentMapMonoid[F[_], G[_]: MonoidK]: Monoid[DependentMap[F, G]] =
    new Monoid[DependentMap[F, G]] {
      override def empty =
        DependentMap.empty[F, G]
      override def combine(x: DependentMap[F, G], y: DependentMap[F, G]) = {
        (x.keys ++ y.keys).toSet.foldLeft(empty) {
          case (dMap, key) => dMap.put(key, (x.get(key).foldK <+> y.get(key).foldK))
        }
      }
    }

  implicit def dependentMapEncoder[F[_], G[_]](
    implicit keyEncoder: KeyEncoder[F[_]],
    dependentEncoder: DependentEncoder[F, G]
  ): Encoder[DependentMap[F, G]] = new Encoder[DependentMap[F, G]] {
    final def apply(m: DependentMap[F, G]) = Json.obj(
      m.iterator.map(pair =>
        keyEncoder(pair.fst) -> dependentEncoder(pair.fst)(pair.snd)
      ).toSeq: _*
    )
  }

  // TODO Really would like to fix up the Decoder.
  // Not sure how to get the types to work out without this auxiliary extra Foo
  // to hold the type skolem

  private case class Foo[F[_], A](fa: F[A]) {
    type Arg = A
  }

  implicit def dependentMapDecoder[F[_], G[_]](
    implicit keyDecoder: KeyDecoder[F[_]],
    dependentDecoder: DependentDecoder[F, G]
  ): Decoder[DependentMap[F, G]] = new Decoder[DependentMap[F, G]] {
    final def apply(c: HCursor): Decoder.Result[DependentMap[F, G]] = {
      // TODO aah replace the get
      c.keys.get.toList.foldM[Decoder.Result, DependentMap[F, G]](DependentMap.empty[F, G]) { (m, keyStr) =>
        import scala.language.existentials
        val key = keyDecoder(keyStr).get // TODO aah replace the get
        val value = dependentDecoder(key).tryDecode(c.downField(keyStr))
        val foo = Foo(key)
        type Arg = foo.Arg
        value.map(v => m.put[Arg](key.asInstanceOf[F[Arg]], v.asInstanceOf[G[Arg]]))
      }
    }
  }

  implicit def dependentMapAt[F[_], G[_], I]: At[DependentMap[F, G], F[I], Option[G[I]]] =
    At[DependentMap[F, G], F[I], Option[G[I]]](
      i => map => map.get(i))(
      i => optV => map => optV.fold(map.remove(i))(v => map.put(i, v))
    )

  implicit def dependentMapIndex[F[_], G[_], I]: Index[DependentMap[F, G], F[I], G[I]] = Index.fromAt
}
