package jjm.syntax

import jjm.Duad

import cats.Functor
import cats.Order
import cats.data.Ior
import cats.implicits._

trait StdSyntax {

  implicit class RichPair[F[_], A, B](val x: F[(A, B)]) { // TODO AnyVal
    def mapFirst[C](f: A => C)(implicit F: Functor[F]): F[(C, B)] =
      x.map { case (a, b) => f(a) -> b }
    def mapSecond[C](f: B => C)(implicit F: Functor[F]): F[(A, C)] =
      x.map { case (a, b) => a -> f(b) }
  }

  implicit class RichList[A](val a: List[A]) { // TODO AnyVal
    def remove(i: Int) = a.take(i) ++ a.drop(i + 1)
  }

  implicit class RichOption[A](val a: Option[A]) { // TODO AnyVal
    // more readable alternatives to forall/exists
    def emptyOr(predicate: A => Boolean): Boolean = a.forall(predicate)
    def nonEmptyAnd(predicate: A => Boolean): Boolean = a.exists(predicate)

    def ifEmpty[B](b: => B): Option[B] = a match {
      case Some(_) => None
      case None    => Some(b)
    }
  }

  implicit class RichMap[A, B](val x: Map[A, B]) { // TODO AnyVal
    def mapVals[C](f: B => C): Map[A, C] = x.transform { case (_, v) => f(v) }
    def zipValues[C](y: Map[A, C]) = {
      val keys = x.keySet.intersect(y.keySet)
      keys.iterator.map(k => k -> (x(k) -> y(k))).toMap
    }
    def normalize(implicit N: Numeric[B]): Map[A, Double] = {
      val total = N.toDouble(x.values.sum)
      mapVals(v => N.toDouble(v) / total)
    }

    // superseded by `Align` once that's in cats
    def merge[C](y: Map[A, C]): Map[A, Ior[B, C]] = {
      val keySet = x.keySet ++ y.keySet
      keySet.iterator.map { key =>
        key -> Ior.fromOptions(x.get(key), y.get(key)).get // should always work
      }.toMap
    }
  }

  final implicit class RichAny[A](val a: A) { // TODO AnyVal
    def <|[B](f: A => B): B = f(a)

    def const: Any => A = _ => a

    def onlyIf(p: (A => Boolean)): Option[A] = Some(a).filter(p)
    def ifNot(p: (A => Boolean)): Option[A] = Some(a).filterNot(p)

    def <->(b: A)(implicit o: Order[A]): Duad[A] = Duad(a, b)
  }

  // implicit class RichTry[A](val t: Try[A]) extends AnyVal {
  //   // TODO probably nope, maybe after adding in log4cats or something
  //   def toOptionLogging(logger: Logger): Option[A] = t match {
  //     case Success(a) =>
  //       Some(a)
  //     case Failure(e) =>
  //       val sw = new StringWriter()
  //       val pw = new PrintWriter(sw, true)
  //       e.printStackTrace(pw)
  //       logger.error(e.getLocalizedMessage + "\n" + sw.getBuffer.toString)
  //       None
  //   }
  // }

  implicit class RichListCompanion[A](val companion: List.type) { // TODO AnyVal

    def unfold[A, B](a: A, f: A => Option[(B, A)]): List[B] = f(a) match {
      case None                   => Nil
      case Some((head, tailToGo)) => head :: unfold(tailToGo, f)
    }

    def unfold[A, B](a: A, f: PartialFunction[A, (B, A)]): List[B] =
      unfold(a, f.lift)
  }
}
