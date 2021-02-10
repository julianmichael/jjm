package jjm.metrics

import cats.Monoid
import cats.implicits._

case class Perplexity(negLogLikelihoods: Vector[Double]) {
  def stats = {
    val nonInfInstances = negLogLikelihoods.filter(_ < Double.PositiveInfinity)
    Perplexity.Stats(
      negLogLikelihoods.size,
      math.exp(negLogLikelihoods.sum / negLogLikelihoods.size),
      nonInfInstances.size,
      math.exp(nonInfInstances.sum   / nonInfInstances.size))
  }
}
object Perplexity {
  def apply(negLogLikelihood: Double): Perplexity = Perplexity(Vector(negLogLikelihood))

  case class Stats(
    numInstances: Int,
    perplexityPerInstance: Double,
    numNonInfInstances: Int,
    perplexityPerNonInfInstance: Double) {
    def metrics: MapTree[String, Metric] = {
      if(numNonInfInstances != numInstances) MapTree.fromPairs(
        "num instances" -> Metric.int(numInstances),
        "perplexity per instance" -> Metric.double(perplexityPerInstance),
        "num non-infinite instances" -> Metric.intOfTotal(numNonInfInstances, numInstances),
        "perplexity per non-infinite instance" -> Metric.double(perplexityPerNonInfInstance)
      ) else MapTree.fromPairs(
        "num instances" -> Metric.int(numInstances),
        "perplexity per instance" -> Metric.double(perplexityPerInstance)
      )
    }
  }

  implicit val perplexityMonoid: Monoid[Perplexity] = {
    import cats.derived.auto.monoid._
    cats.derived.semiauto.monoid
  }

  implicit val perplexityHasMetrics = new HasMetrics[Perplexity] {
    def getMetrics(counts: Perplexity) = counts.stats.metrics
  }
}
