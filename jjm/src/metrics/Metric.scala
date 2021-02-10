package jjm.metrics

sealed trait Metric {
  // TODO make this a separate routine, possibly use as an implicit.
  // it'd be nice to make rendering configurable
  import Metric._
  def render: String = this match {
    case MetricMetadata(s) => s
    case MetricBool(x) => s"$x"
    case MetricDouble(x) => f"$x%.3f"
    case MetricInt(x) => f"$x%d"
    case MetricIntOfTotal(value, total) => f"$value%d (${value.toDouble / total * 100}%.2f%%)"
  }
}
object Metric {
  case class MetricBool(value: Boolean) extends Metric
  case class MetricDouble(value: Double) extends Metric
  case class MetricInt(value: Int) extends Metric
  case class MetricIntOfTotal(value: Int, total: Int) extends Metric
  case class MetricMetadata(value: String) extends Metric

  def bool(value: Boolean): Metric = MetricBool(value)
  def int(value: Int): Metric = MetricInt(value)
  def double(value: Double): Metric = MetricDouble(value)
  def intOfTotal(value: Int, total: Int): Metric = MetricIntOfTotal(value, total)
  def metadata(value: String): Metric = MetricMetadata(value)
}
