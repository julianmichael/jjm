package jjm

import cats.~>
import cats.Id
import cats.FlatMap
import cats.implicits._

trait DotKleisli[F[_], A <: Dot] { self =>
  def apply(a: A): F[a.Out]

  // TODO: compose (flat), composeF (nested), composeM (monadic), ...?
  // can these even be implemented? bc of the issue of having a value-level map between Dots

  // TODO sequencing and binding under the dot

  // TODO pair results
  // def product[G[_]]

  def andThenK[G[_]](trans: F ~> G): DotKleisli[G, A] =
    new DotKleisli[G, A] { def apply(a: A): G[a.Out] = trans(self(a)) }

  // TODO this needs a better name
  def andThenDotK[G[_]](dotTrans: DotKleisli[λ[B => F[B] => G[B]], A]): DotKleisli[G, A] =
    new DotKleisli[G, A] { def apply(a: A): G[a.Out] = dotTrans(a)(self(a)) }

  // TODO composeDotK
  
  def dotFlatMap[G[_]](
    f: DotKleisli[λ[B => B => F[G[B]]], A])(
    implicit fm: FlatMap[F]
  ): DotKleisli[λ[B => F[G[B]]], A] =
    new DotKleisli[λ[B => F[G[B]]], A] { def apply(a: A): F[G[a.Out]] = self(a).flatMap(f(a)) }

  def dotFlatTap(
    f: DotKleisli[λ[B => B => F[Unit]], A])(
    implicit fm: FlatMap[F]
  ): DotKleisli[F, A] =
    new DotKleisli[F, A] { def apply(a: A): F[a.Out] = self(a).flatTap(f(a)) }

  def toFunctionK: DotFunctionK[F, A] = new DotFunctionK[F, A] {
    def apply[B](dot: A { type Out = B }) = self(dot)
  }

  def toFunction[B](
    f: DotKleisli[λ[A => F[A] => B], A]
  ): A => B = (a: A) => f(a)(self(a))

  def toFunction[B](
    f: F ~> λ[A => B]
  ): A => B = (a: A) => f(self(a))

  def toDotKleisliGraph(implicit fin: Finite[A]) =
    DotKleisliGraph.fromDotKleisli(this)
}

object DotKleisli {
  val unit = new DotKleisli[Id, DotUnit] {
    override def apply(a: DotUnit) = ()
  }

  def fromFunctionK[F[_], A <: Dot](f: DotFunctionK[F, A]) =
    new DotKleisli[F, A] {
      @inline def apply(a: A): F[a.Out] = _apply[a.Out](a)
      @inline private[this] def _apply[B](req: A { type Out = B }): F[B] = f(req)
    }

  // TODO does this make any sense

  // type Const[C, A] = C

  // def const[F[_], A <: Dot, C](c: F[C]) = new DotKleisli[Lambda[B => F[Const[C, ?]]], A] {
  //   def apply(a: A): Const[C, a.Out] = c
  // }

  // TODO: typeclass instances
}
