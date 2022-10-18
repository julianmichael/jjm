package jjm.metrics

import cats.Monoid
import cats.implicits._

case class Counts(hist: Map[Int, Int]) {

  def histogramString(totalBarLength: Int = 25) = {
    val total = hist.values.sum
    hist.values.toList.maximumOption.fold("<empty histogram>") { max =>
      hist.toList.sortBy(_._1).map { case (num, count) =>
        val numPounds = math.round(count.toDouble * totalBarLength / max).toInt
        val poundString = "#" * numPounds
        val spacer = " " * (totalBarLength - numPounds)
        val percent = count * 100.0 / total
        f"$num%7d | $poundString%s$spacer%s $count%7d ($percent%.2f%%)"
      }.mkString("\n")
    }
  }

  def stats = {
    val numInstances = hist.map(_._2).sum
    val sum = hist.map(Function.tupled(_ * _)).sum
    val mean = sum.toDouble / numInstances
    val variance = hist.map {
      case (value, count) => count * math.pow(value - mean, 2)
    }.sum / numInstances
    val median = {
      val nums = hist.toList.sortBy(_._1).foldMap {
        case (num, count) => Vector.fill(count)(num)
      }
      if(numInstances % 2 == 0) {
        (nums((numInstances / 2) - 1) + nums(numInstances / 2)) / 2.0
      } else nums(numInstances / 2)
    }
    // TODO slightly more efficient
    // val median = hist.toList.sortBy(_._1).foldLeft(
    //   ((numInstances / 2), (numInstances % 2 == 0), None)) {
    //   case ((remaining, isEven, prevNumOpt), (nextNum, nextCount)) =>
    //     if(remaining == 0) {
    //       ...
    //     }
    // }
    Counts.Stats(
      numInstances, sum,
      median,
      mean, variance,
      hist.keySet.min,
      hist.keySet.max)
  }
}
object Counts {
  def apply[A](n: Int): Counts = Counts(Map(n -> 1))

  case class Stats(
    numInstances: Int,
    sum: Int,
    median: Double,
    mean: Double,
    variance: Double,
    min: Int,
    max: Int
  ) {
    def stdev = math.sqrt(variance)

    def metrics: MapTree[String, Metric] = MapTree.fromPairs(
      "num instances" -> Metric.int(numInstances),
      "sum" -> Metric.int(sum),
      "mean" -> Metric.double(mean),
      "median" -> Metric.double(median),
      // "variance" -> Metric.double(variance),
      "stdev" -> Metric.double(stdev),
      "min" -> Metric.int(min),
      "max" -> Metric.int(max),
    )
  }

  implicit val countsMonoid: Monoid[Counts] = {
    import cats.derived.auto.monoid._
    cats.derived.semiauto.monoid
  }

  implicit val countsHasMetrics = new HasMetrics[Counts] {
    def getMetrics(counts: Counts) = counts.stats.metrics
  }
}
