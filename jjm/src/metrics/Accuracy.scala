package jjm.metrics

import cats.Monoid
import cats.implicits._

case class Accuracy[A](
  correct: Vector[A] = Vector(),
  incorrect: Vector[A] = Vector()) {
  def stats = Accuracy.Stats(correct.size, incorrect.size)
}
object Accuracy {
  def correct[A](as: A*) = Accuracy(correct = as.toVector)
  def incorrect[A](as: A*) = Accuracy(incorrect = as.toVector)
  case class Stats(
    correct: Int = 0,
    incorrect: Int = 0
  ) {
    def predicted = correct + incorrect
    def accuracy = correct.toDouble / predicted

    def getTree: MapTree[String, Metric] = MapTree.fromPairs(
      "num predicted" -> Metric.int(predicted),
      "accuracy" -> Metric.double(accuracy),
      )
  }
  object Stats {
    implicit val accuracyStatsMonoid: Monoid[Accuracy.Stats] = {
      import cats.derived.auto.monoid._
      cats.derived.semiauto.monoid
    }
    implicit val accuracyStatsHasMetrics = new HasMetrics[Accuracy.Stats] {
      def getMetrics(acc: Accuracy.Stats) = acc.getTree
    }
  }
  implicit def accuracyMonoid[A]: Monoid[Accuracy[A]] = {
    import cats.derived.auto.monoid._
    cats.derived.semiauto.monoid
  }
  implicit def accuracyHasMetrics[A] = new HasMetrics[Accuracy[A]] {
    def getMetrics(acc: Accuracy[A]) = acc.stats.getTree
  }
}
