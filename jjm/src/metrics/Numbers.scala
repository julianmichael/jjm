package jjm.metrics

import cats.Monoid
import cats.implicits._

case class Numbers[N](values: Vector[N])(implicit N: Numeric[N]) {
  def stats = {
    val dValues = values.map(N.toDouble)
    val sum = dValues.sum
    val mean = sum / values.size
    Numbers.Stats(
      values.size,
      sum, mean,
      dValues.iterator.map(x => math.pow(x - mean, 2)).sum / values.size,
      Quartiles.fromValues(dValues))
  }
}
object Numbers {
  def apply[N: Numeric](n: N): Numbers[N] = Numbers(Vector(n))
  case class Stats(
    count: Int,
    sum: Double,
    mean: Double,
    variance: Double,
    quartiles: Quartiles
  ) {
    def stdev = math.sqrt(variance)
    def getMetrics: MapTree[String, Metric] = MapTree.fork(
      "count" ->     MapTree.leaf[String](Metric.int(count)),
      "sum" ->       MapTree.leaf[String](Metric.double(sum)),
      "mean" ->      MapTree.leaf[String](Metric.double(mean)),
      "variance" ->  MapTree.leaf[String](Metric.double(variance)),
      "stdev" ->     MapTree.leaf[String](Metric.double(stdev)),
      "quartiles" -> quartiles.getMetrics
    )
  }

  implicit def numbersMonoid[A: Numeric]: Monoid[Numbers[A]] = {
    import cats.derived.auto.monoid._
    cats.derived.semiauto.monoid
  }
  implicit def numbersHasMetrics[A] = new HasMetrics[Numbers[A]] {
    def getMetrics(nums: Numbers[A]) = nums.stats.getMetrics
  }
}
