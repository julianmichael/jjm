package jjm.syntax

import jjm.DependentMap
import jjm.DotMap
import jjm.DotF

import cats.~>
import cats.Applicative
import cats.Foldable
import cats.Order
import cats.Monad
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

  }

  implicit class RichNonEmptyList[A](val as: NonEmptyList[A]) { // TODO AnyVal

    def partition[B, C](f: A => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]] = {
      val init = f(as.head).fold(
        b => Ior.Left(NonEmptyList.of(b)),
        c => Ior.Right(NonEmptyList.of(c))
      ): Ior[NonEmptyList[B], NonEmptyList[C]]
      as.tail.map(f).foldLeft(init) {
        case (Ior.Left(bs), Left(b))      => Ior.Left(b :: bs)
        case (Ior.Right(cs), Left(b))     => Ior.Both(NonEmptyList.of(b), cs)
        case (Ior.Both(bs, cs), Left(b))  => Ior.Both(b :: bs, cs)
        case (Ior.Left(bs), Right(c))     => Ior.Both(bs, NonEmptyList.of(c))
        case (Ior.Right(cs), Right(c))    => Ior.Right(c :: cs)
        case (Ior.Both(bs, cs), Right(c)) => Ior.Both(bs, c :: cs)
      }
    }

    // taken from latest cats; holdover until version upgrade
    def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyList[AA] = {
      NonEmptyList.fromListUnsafe(as.toList.sorted(AA.toOrdering))
    }

    // taken from latest cats; holdover until version upgrade
    def init: List[A] = as.tail match {
      case Nil => Nil
      case t   => as.head :: t.init
    }

    // taken from latest cats; holdover until version upgrade
    def last: A = as.tail.lastOption match {
      case None    => as.head
      case Some(a) => a
    }

  }

  implicit class RichNonEmptyListCompanion(val nel: NonEmptyList.type) { // TODO AnyVal

    // taken from latest cats; holdover until version upgrade
    def ofInitLast[A](init: List[A], last: A): NonEmptyList[A] =
      init match {
        case Nil    => NonEmptyList(last, Nil)
        case h :: t => NonEmptyList(h, t :+ last)
      }
  }

  // taken from latest cats; holdover until version upgrade
  implicit class RichArrow[F[_, _], A, B](val f: F[A, B])(implicit F: Arrow[F]) {

    def &&&[C](g: F[A, C]): F[A, (B, C)] = {
      F.andThen(F.lift((x: A) => (x, x)), F.split(f, g))
    }
  }

  implicit class RichIorSame[A](val ior: Ior[A, A]) { // TODO AnyVal
    def mergeM[M[_]: Monad](f: (A, A) => M[A]) = ior.fold(
      Monad[M].pure,
      Monad[M].pure,
      f
    )
  }

  implicit class CatsRichMap[A, B](val x: Map[A, B]) { // TODO AnyVal
    // superseded by `Align` once that's in cats
    def merge[C](y: Map[A, C]): Map[A, Ior[B, C]] = {
      val keySet = x.keySet ++ y.keySet
      keySet.iterator.map { key =>
        key -> Ior.fromOptions(x.get(key), y.get(key)).get // should always work
      }.toMap
    }
  }
}
