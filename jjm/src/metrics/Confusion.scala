package jjm.metrics

import cats.Monoid
import cats.MonoidK
import cats.Show
import cats.implicits._

// outer layer is gold
case class Confusion[A, I](
  matrix: Map[A, Map[A, List[I]]]
) {

  def stats = Confusion.Stats(
    matrix.map { case (k1, v1) =>
      k1 -> v1.map { case (k2, v2) =>
        k2 -> v2.size
      }
    }
  )
}
object Confusion {

  case class Stats[A](
    matrix: Map[A, Map[A, Int]]
  ) {

    def allLabels = matrix.values.flatMap(_.keys).toSet ++ matrix.keySet
    def goldCounts = allLabels.map(k => k -> matrix.get(k).fold(0)(_.values.toList.combineAll)).toMap
    def predCounts = allLabels.map(k => k -> matrix.values.map(_.get(k).getOrElse(0)).toList.combineAll).toMap
    def total = matrix.values.map(_.values.sum).sum

    def prettyString(classFreqBound: Int)(implicit s: Show[A]) = {
      val gCounts = goldCounts
      val pCounts = predCounts
      val tot = total
      val emptyMap = Map.empty[A, Int]
      val sortedFilteredClasses = gCounts.keys.toList
        .filter(k => (gCounts(k) + pCounts(k)) >= classFreqBound)
        .sortBy(k => -gCounts(k) - pCounts(k))
      val header = (" " :: sortedFilteredClasses.map(_.show)).map(pred => f"$pred%s").mkString(",")
      val body = sortedFilteredClasses.map { gold =>
        f"${gold.show}%s," + sortedFilteredClasses.map { pred =>
          val count = matrix.get(gold).getOrElse(emptyMap).get(pred).getOrElse(0)
          val prop = count.toDouble / tot
          f"$count%d"
        }.mkString(",")
      }.mkString("\n")
      header + "\n" + body
    }

    // def numPredicted = tp + fp

    // def metrics: MapTree[String, Metric] = MapTree.fromPairs(
    //   "num gold" -> Metric.int(numGold),
    //   "num predicted" -> Metric.int(numPredicted),
    //   "precision" -> Metric.double(precision),
    //   "recall" -> Metric.double(recall),
    //   "f1" -> Metric.double(f1)
    // )
  }
  object Stats {
    // implicit val confStatsMonoid = new Monoid[Stats] {
    //   def empty: Stats = Stats()
    //   def combine(x: Stats, y: Stats): Stats =
    //     Stats(x.tp + y.tp, x.tn + y.tn, x.fp + y.fp, x.fn + y.fn)
    // }
    // implicit val confStatsHasMetrics = new HasMetrics[Stats] {
    //   def getMetrics(stats: Stats) = stats.metrics
    // }
  }

  implicit def confusionMonoidK[A]: MonoidK[Confusion[A, *]] = new MonoidK[Confusion[A, *]] {
    override def empty[I] = Confusion[A, I](Map())
    override def combineK[I](x: Confusion[A, I], y: Confusion[A, I]) = Confusion[A, I](x.matrix |+| y.matrix)
  }
  implicit def confusionMonoid[A, I]: Monoid[Confusion[A, I]] = confusionMonoidK.algebra[I]
  // implicit def confAHasMetrics[A] = new HasMetrics[BinaryConf[A]] {
  //   def getMetrics(conf: BinaryConf[A]) = conf.stats.metrics
  // }

  def instance[A, I](gold: A, pred: A, value: I) = Confusion(Map(gold -> Map(pred -> List(value))))
}
