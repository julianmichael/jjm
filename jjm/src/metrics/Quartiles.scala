package jjm.metrics

case class Quartiles(
  min: Double,
  firstQuartile: Double,
  median: Double,
  thirdQuartile: Double,
  max: Double
) {
  def getMetrics: MapTree[String, Metric] = MapTree.fromPairs(
    "min" -> Metric.double(min),
    "first quartile" -> Metric.double(firstQuartile),
    "median" -> Metric.double(median),
    "third quartile" -> Metric.double(thirdQuartile),
    "max" -> Metric.double(max)
  )
}
object Quartiles {
  def fromValues(values: Vector[Double]) = {
    val sortedValues = values.sorted
    def get(frac: Double) = sortedValues(math.max(0, (frac * values.size).toInt - 1))
    Quartiles(get(0.0), get(0.25), get(0.5), get(0.75), get(1.00))
  }
  implicit val quartilesHasMetrics: HasMetrics[Quartiles] = new HasMetrics[Quartiles] {
    def getMetrics(q: Quartiles) = q.getMetrics
  }
}
