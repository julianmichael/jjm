package jjm.syntax

import jjm.DependentMap
import jjm.DotMap
import jjm.DotF

import cats.~>
import cats.Applicative
import cats.Foldable
import cats.Order
import cats.Monad
import cats.Monoid
import cats.data.Ior
import cats.data.NonEmptyList
import cats.arrow.Arrow
import cats.implicits._

trait CatsSyntax {

  implicit class RichFoldable[F[_]: Foldable, A](val fa: F[A]) {

    def counts: Map[A, Int] = fa.foldMap(a => Map(a -> 1))

    def sum(implicit N: Numeric[A]): A = fa.foldLeft(N.fromInt(0))(N.plus)

    def meanOpt(implicit N: Numeric[A]): Option[Double] = {
      val (sum, count) = fa.foldLeft(N.fromInt(0), N.fromInt(0)) {
        case ((curSum, curCount), a) => (N.plus(curSum, a), N.plus(curCount, N.fromInt(1)))
      }
      if (count == 0) None else Some(N.toDouble(sum) / N.toDouble(count))
    }

    def proportion(predicate: A => Boolean): Double =
      fa.foldLeft((0, 0)) {
        case ((trues, total), a) =>
          if (predicate(a)) (trues + 1, total + 1)
          else (trues, total + 1)
      } match { case (trues, total) => trues.toDouble / total }

    def maxima(implicit o: Order[A]): List[A] =
      fa.foldLeft(List.empty[A]) { (maxes, a) =>
        maxes.headOption.fold(a :: Nil) { max =>
          if(a > max) a :: Nil
          else if(a.eqv(max)) a :: maxes
          else maxes
        }
      }.reverse

    def maximaBy[B: Order](f: A => B): List[A] =
      fa.foldLeft(List.empty[(A, B)]) { (maxes, a) =>
        val key = f(a)
        maxes.headOption.fold((a -> key) :: Nil) { case (_, maxKey) =>
          if(key > maxKey) (a -> key) :: Nil
          else if(key < maxKey) maxes
          else (a -> key) :: maxes
        }
      }.reverse.map(_._1)

    def minima(implicit o: Order[A]): List[A] =
      maxima(Order.reverse(o))

    def minimaBy[B](f: A => B)(implicit o: Order[B]): List[A] =
      maximaBy(f)(Order.reverse(o))
  }

  implicit class RichIorSame[A](val ior: Ior[A, A]) { // TODO AnyVal
    def mergeM[F[_]: Applicative](f: (A, A) => F[A]) = ior.fold(
      Applicative[F].pure,
      Applicative[F].pure,
      f
    )
  }

  // TODO
  // implicit class RichNonEmptyList[A](val x: NonEmptyList[A]) { // TODO AnyVal
  //   def maximaNel(implicit o: Order[A]): NonEmptyList[A]
  //   def maximaNelBy[B](f: A => B)(implicit o: Order[B]): NonEmptyList[A]
  //   def minimaNel(implicit o: Order[A]): NonEmptyList[A] =
  //     maximaNel(o.reverse)
  //   def minimaNelBy[B](f: A => B)(implicit o: Order[B]): NonEmptyList[A] =
  //     maximaNelBy(f)(o.reverse)
  // }

  implicit class RichNested[M[_], F[_], A](val x: M[F[A]]) { // TODO AnyVal
    def flatFoldMapM[B: Monoid](f: A => M[B])(implicit M: Monad[M], F: Foldable[F]): M[B] =
      M.flatMap(x)(_.foldMapM(f))
  }
}
