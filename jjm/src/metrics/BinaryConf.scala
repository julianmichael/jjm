package jjm.metrics

import io.circe.generic.JsonCodec

import cats.Monoid
import cats.MonoidK
import cats.implicits._

case class BinaryConf[A](
  tp: Vector[A] = Vector(),
  tn: Vector[A] = Vector(),
  fp: Vector[A] = Vector(),
  fn: Vector[A] = Vector()
) {

  def stats = BinaryConf.Stats(
    tp.size, tn.size, fp.size, fn.size
  )
}
object BinaryConf {

  @JsonCodec case class Stats(
    tp: Int = 0,
    tn: Int = 0,
    fp: Int = 0,
    fn: Int = 0
  ) {

    def numPredicted = tp + fp

    def numGold = tp + fn

    def numInstances = tp + tn + fp + fn
    def numPositiveInstances = tp + fp + fn

    def precision = if(tp + fp > 0) {
      tp.toDouble / (tp + fp)
    } else 1.0

    def recall = if(tp + fn > 0) {
      tp.toDouble / (tp + fn)
    } else 0.0

    def f1 = Functions.harmonicMean(precision, recall)

    def fMeasure(beta: Double) = Functions.weightedHarmonicMean(beta, precision, recall)

    def metrics: MapTree[String, Metric] = MapTree.fromPairs(
      "num gold" -> Metric.int(numGold),
      "num predicted" -> Metric.int(numPredicted),
      "precision" -> Metric.double(precision),
      "recall" -> Metric.double(recall),
      "f1" -> Metric.double(f1)
    )
  }
  object Stats {
    implicit val confStatsMonoid = new Monoid[Stats] {
      def empty: Stats = Stats()
      def combine(x: Stats, y: Stats): Stats =
        Stats(x.tp + y.tp, x.tn + y.tn, x.fp + y.fp, x.fn + y.fn)
    }
    implicit val confStatsHasMetrics = new HasMetrics[Stats] {
      def getMetrics(stats: Stats) = stats.metrics
    }
  }

  implicit val confAMonoidK: MonoidK[BinaryConf] = {
    import cats.derived.auto.monoidK._
    cats.derived.semiauto.monoidK
  }
  implicit def confAMonoid[A]: Monoid[BinaryConf[A]] = confAMonoidK.algebra[A]
  implicit def confAHasMetrics[A] = new HasMetrics[BinaryConf[A]] {
    def getMetrics(conf: BinaryConf[A]) = conf.stats.metrics
  }

  def tp[A](a: A) = BinaryConf[A](tp = Vector(a))
  def tn[A](a: A) = BinaryConf[A](tn = Vector(a))
  def fp[A](a: A) = BinaryConf[A](fp = Vector(a))
  def fn[A](a: A) = BinaryConf[A](fn = Vector(a))

  def fromSets[A](
    gold: Set[A],
    pred: Set[A],
    all: Set[A]
  ): BinaryConf[A] = BinaryConf[A](
    tp = gold.intersect(pred).toVector,
    tn = (all -- pred -- gold).toVector,
    fp = (pred -- gold).toVector,
    fn = (gold -- pred).toVector
  )

  def fromSets[A](
    gold: Set[A],
    pred: Set[A]
  ): BinaryConf[A] = fromSets(gold, pred, gold ++ pred)
}
