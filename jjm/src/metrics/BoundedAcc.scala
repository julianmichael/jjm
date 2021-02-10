package jjm.metrics

import cats.Monoid
import cats.MonoidK
import cats.implicits._

case class BoundedAcc[A](
  correct:   Vector[A] = Vector(),
  incorrect: Vector[A] = Vector(),
  uncertain: Vector[A] = Vector()
) {
  def stats = BoundedAcc.Stats(
    correct.size,
    incorrect.size,
    uncertain.size
  )
}
object BoundedAcc {
  implicit val boundedAccMonoidK: MonoidK[BoundedAcc] = {
    import cats.derived.auto.monoidK._
    cats.derived.semiauto.monoidK
  }
  implicit def boundedAccMonoid[A]: Monoid[BoundedAcc[A]] = boundedAccMonoidK.algebra[A]
  implicit def boundedAccHasMetrics[A] = new HasMetrics[BoundedAcc[A]] {
    def getMetrics(bacc: BoundedAcc[A]) = bacc.stats.metrics
  }

  def correct[A](a: A) = BoundedAcc[A](correct = Vector(a))
  def incorrect[A](a: A) = BoundedAcc[A](incorrect = Vector(a))
  def uncertain[A](a: A) = BoundedAcc[A](uncertain = Vector(a))

  case class Stats(
    correct: Int = 0,
    incorrect: Int = 0,
    uncertain: Int = 0
  ) {
    def predicted = correct + incorrect + uncertain
    def accuracyLowerBound = correct.toDouble / predicted
    def accuracyUpperBound = (correct + uncertain).toDouble / predicted

    def metrics: MapTree[String, Metric] = MapTree.fromPairs(
      "num predicted" -> Metric.int(predicted),
      "acc-lb" -> Metric.double(accuracyLowerBound),
      "acc-ub" -> Metric.double(accuracyUpperBound)
    )
  }
  object Stats {
    def correct(n: Int = 1) = Stats(correct = n)
    def incorrect(n: Int = 1) = Stats(incorrect = n)
    def uncertain(n: Int = 1) = Stats(uncertain = n)
    implicit val boundedAccStatsMonoid: Monoid[Stats] = {
      import cats.derived.auto.monoid._
      cats.derived.semiauto.monoid
    }
    implicit val boundedAccStatsHasMetrics = new HasMetrics[Stats] {
      def getMetrics(stats: Stats) = stats.metrics
    }
  }
}
