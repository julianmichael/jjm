package jjm.metrics

import cats.Functor
import cats.Monoid
import cats.MonoidK
import cats.implicits._

case class Count[A](values: Vector[A]) {
  def stats = Count.Stats(values.size, values.toSet.size)
  // TODO functorfilter
  def filter(f: A => Boolean) = Count(values.filter(f))
}
object Count {
  def apply[A](a: A): Count[A] = Count(Vector(a))

  case class Stats(numInstances: Int, numTypes: Int) {
    def metrics: MapTree[String, Metric] = MapTree.fromPairs(
      "num types" -> Metric.int(numTypes),
      "num instances" -> Metric.int(numInstances),
    )
  }

  implicit val countFunctor: Functor[Count] = new Functor[Count] {
    def map[A, B](fa: Count[A])(f: A => B): Count[B] = Count(fa.values.map(f))
  }

  implicit val countMonoidK: MonoidK[Count] = {
    import cats.derived.auto.monoidK._
    cats.derived.semiauto.monoidK
  }

  implicit def countMonoid[A]: Monoid[Count[A]] = countMonoidK.algebra[A]

  implicit def countHasMetrics[A] = new HasMetrics[Count[A]] {
    def getMetrics(conf: Count[A]) = conf.stats.metrics
  }
}
