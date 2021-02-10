package jjm.metrics

import cats.Functor
import cats.Monoid
import cats.MonoidK
import cats.Show
import cats.implicits._

case class FewClassCount[A](values: Vector[A]) {
  def stats = FewClassCount.Stats(values.groupBy(x => x).map { case (k, v) => k -> v.size })
  // TODO functorfilter
  def filter(f: A => Boolean) = FewClassCount(values.filter(f))
}
object FewClassCount {
  def apply[A](a: A): FewClassCount[A] = FewClassCount(Vector(a))

  case class Stats[A](counts: Map[A, Int]) {
    val total = counts.values.sum
    def metrics(implicit aShow: Show[A]): MapTree[String, Metric] = MapTree.fromPairs(
      counts.toList.map { case (a, c) => a.show -> Metric.intOfTotal(c, total) }: _*
    )
  }

  implicit val fewClassCountFunctor: Functor[FewClassCount] = new Functor[FewClassCount] {
    def map[A, B](fa: FewClassCount[A])(f: A => B): FewClassCount[B] = FewClassCount(fa.values.map(f))
  }

  implicit val fewClassCountMonoidK: MonoidK[FewClassCount] = {
    import cats.derived.auto.monoidK._
    cats.derived.semiauto.monoidK
  }

  implicit def fewClassCountMonoid[A]: Monoid[FewClassCount[A]] = fewClassCountMonoidK.algebra[A]

  implicit def fewClassCountountHasMetrics[A: Show] = new HasMetrics[FewClassCount[A]] {
    def getMetrics(conf: FewClassCount[A]) = conf.stats.metrics
  }
}
