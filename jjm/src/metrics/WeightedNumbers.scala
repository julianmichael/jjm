package jjm.metrics

import cats.Monoid
import cats.implicits._


// TODO weighted quartiles, etc.
// TODO make this a CommutativeMonoid

case class WeightedNumbers[N](values: Vector[(Double, N)])(implicit N: Numeric[N]) {
  def stats = {
    val pcount = values.foldMap(_._1)
    val wsum = values.foldMap { case (w, n) => w * N.toDouble(n) }
    WeightedNumbers.Stats(
      pcount, wsum
    )
  }
  def normalize = {
    val pcount = values.foldMap(_._1)
    WeightedNumbers(
      values.map { case (w, n) =>
        val normedWeight = if(pcount == 0.0) 0.0 else w / pcount
        normedWeight -> n
      }
    )
  }
}
object WeightedNumbers {
  def apply[N: Numeric](n: N, weight: Double = 1.0): WeightedNumbers[N] = WeightedNumbers(Vector(weight -> n))
  case class Stats(
    pseudocount: Double,
    weightedSum: Double,
  ) {
    def weightedMean: Double = if(pseudocount == 0.0) 0.0 else weightedSum / pseudocount
    def getMetrics: MapTree[String, Metric] = MapTree.fork(
      "pseudocount" ->     MapTree.leaf[String](Metric.double(pseudocount)),
      "weighted sum" ->    MapTree.leaf[String](Metric.double(weightedSum)),
      "weighted mean" ->   MapTree.leaf[String](Metric.double(weightedMean))
    )
  }

  implicit def weightedNumbersMonoid[A: Numeric]: Monoid[WeightedNumbers[A]] = {
    new Monoid[WeightedNumbers[A]] {
      override def empty: WeightedNumbers[A] = WeightedNumbers[A](Vector())
      override def combine(x: WeightedNumbers[A], y: WeightedNumbers[A]): WeightedNumbers[A] = {
        WeightedNumbers(x.values ++ y.values)
      }
    }
  }
  implicit def weightedNumbersHasMetrics[A] = new HasMetrics[WeightedNumbers[A]] {
    def getMetrics(nums: WeightedNumbers[A]) = nums.stats.getMetrics
  }
}
