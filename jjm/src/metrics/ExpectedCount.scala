package jjm.metrics

import cats.Monoid
import cats.implicits._

case class ExpectedCount(
  expectedTotal: Double,
  expectedNumInstances: Double
) {
  def expectationPerInstance = expectedTotal / expectedNumInstances
  def metrics: MapTree[String, Metric] = MapTree.fromPairs(
    "expected total" -> Metric.double(expectedTotal),
    "expected num instances" -> Metric.double(expectedNumInstances),
    "expectation per instance" -> Metric.double(expectationPerInstance)
  )
}
object ExpectedCount {
  def apply(expectedCount: Double): ExpectedCount = ExpectedCount(expectedCount, 1.0)
  implicit val expectedCountMonoid: Monoid[ExpectedCount] = {
    import cats.derived.auto.monoid._
    cats.derived.semiauto.monoid
  }
  implicit val expectedCountHasMetrics = new HasMetrics[ExpectedCount] {
    def getMetrics(ec: ExpectedCount) = ec.metrics
  }
}
