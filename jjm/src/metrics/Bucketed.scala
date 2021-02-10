package jjm.metrics

import cats.Applicative
import cats.Eval
import cats.Monoid
import cats.Traverse
import cats.implicits._

import monocle.function.Each

import HasMetrics.ops._

case class Bucketed[MetricData](
  data: Map[Map[String, String], MetricData]
) {
  def mapBucketValues(key: String, mapper: String => String)(implicit M: Monoid[MetricData]) = Bucketed(
    data.toList.map {
      case (buckets, m) => buckets.get(key).fold(buckets)(v => buckets + (key -> mapper(v))) -> m
    }.groupBy(_._1).map {
      case (newBuckets, allMetrics) => newBuckets -> allMetrics.map(_._2).combineAll
    }
  )

  def collapseBuckets(keys: String*)(implicit M: Monoid[MetricData]): Bucketed[MetricData] = collapseBuckets(keys.toList)

  def collapseBuckets(keys: List[String])(implicit M: Monoid[MetricData]): Bucketed[MetricData] = Bucketed(
    data.toList.map {
      case (buckets, m) => keys.foldLeft(buckets)(_ - _) -> m
    }.groupBy(_._1).map {
      case (newBuckets, allMetrics) => newBuckets -> allMetrics.map(_._2).combineAll
    }
  )

  def filter(p: MetricData => Boolean): Bucketed[MetricData] = Bucketed(
    data.collect { case (k, v) if p(v) => k -> v }
  )

  def collapsed(implicit M : Monoid[MetricData]) = data.values.toList.combineAll
}
object Bucketed {
  // TODO traverseFilter
  implicit val bucketedTraverse: Traverse[Bucketed] = new Traverse[Bucketed] {
    def traverse[G[_]: Applicative, A, B](fa: Bucketed[A])(f: A => G[B]): G[Bucketed[B]] = {
      fa.data.toList.traverse { case (bucket, value) => f(value).map(bucket -> _) }.map(l => Bucketed(l.toMap))
    }
    def foldLeft[A, B](fa: Bucketed[A], b: B)(f: (B, A) => B): B = fa.data.values.foldLeft(b)(f)
    def foldRight[A, B](fa: Bucketed[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.data.values.foldRight(lb)(f)
  }
  implicit def bucketedMonoid[A : Monoid]: Monoid[Bucketed[A]] = {
    import cats.derived.auto.monoid._
    cats.derived.semiauto.monoid
  }
  // TODO perhaps draw out the proportion calculation into a separate method so user can decide when to do it
  implicit def bucketedHasMetrics[A : HasMetrics : Monoid]: HasMetrics[Bucketed[A]] = new HasMetrics[Bucketed[A]] {
    def getMetrics(ba: Bucketed[A]): MapTree[String, Metric] = {
      val collapsedMetrics = ba.collapsed.getMetrics
      val computeIntsOfTotal = (x: Metric, y: Metric) => (x, y) match {
        case (Metric.MetricInt(xi), Metric.MetricInt(yi)) => Metric.intOfTotal(xi, yi)
        case (x, _) => x
      }
      MapTree.fork(
        ba.data.map { case (bucketSpec, bucketData) =>
          val keyStr = "{ " + bucketSpec.toList.sortBy(_._1).map { case (k, v) => s"$k: $v"}.mkString(", ") + " }"
          keyStr -> bucketData.getMetrics.merge(collapsedMetrics, computeIntsOfTotal)
        }
      )
    }
  }
  implicit def bucketedEach[A] = Each.fromTraverse[Bucketed, A]
}
