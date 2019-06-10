package jjm

import cats.Applicative
import cats.Monad
import cats.StackSafeMonad
import cats.implicits._

import monocle.Iso
import monocle.macros.GenPrism

sealed trait OrWrapped[F[_], A] {
  import OrWrapped._
  def cata[B](pure: A => B, wrapped: F[A] => B): B = this match {
    case Pure(a) => pure(a)
    case Wrapped(fa) => wrapped(fa)
  }

  def wrap(f: A => F[A]): F[A] = cata(f, identity)
  def wrapPure(implicit A: Applicative[F]): F[A] = cata(A.pure, identity)
}

object OrWrapped {
  case class Pure[F[_], A](value: A) extends OrWrapped[F, A]
  object Pure {
    def value[F[_], A] = Iso[Pure[F, A], A](_.value)(Pure.apply[F, A])
  }
  case class Wrapped[F[_], A](wrappedValue: F[A]) extends OrWrapped[F, A]
  object Wrapped {
    def wrappedValue[F[_], A] = Iso[Wrapped[F, A], F[A]](_.wrappedValue)(Wrapped.apply[F, A])
  }

  def pure[F[_], A] = GenPrism[OrWrapped[F, A], Pure[F, A]].composeIso(Pure.value)
  def wrapped[F[_], A] = GenPrism[OrWrapped[F, A], Wrapped[F, A]].composeIso(Wrapped.wrappedValue)

  def pure[F[_]] = new PureBuilder[F]
  class PureBuilder[F[_]] {
    def apply[A](value: A): OrWrapped[F, A] = Pure[F, A](value)
  }

  // TODO is this actually stack-safe? why did I do this instead of implementing tailRecM?
  implicit def catsOrWrappedMonad[F[_]: Monad]: Monad[OrWrapped[F, ?]] =
    new StackSafeMonad[OrWrapped[F, ?]] {

      override def pure[A](a: A): OrWrapped[F, A] = OrWrapped.pure[F](a)

      override def flatMap[A, B](
        owa: OrWrapped[F, A])(
        f: A => OrWrapped[F, B]
      ): OrWrapped[F, B] = owa.cata(
        f, (fa: F[A]) => OrWrapped.wrapped(fa.flatMap(a => f(a).wrapPure))
      )
    }

}
