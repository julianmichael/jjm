package jjm.metrics

import cats.Show
import cats.implicits._

import simulacrum._
import scala.language.implicitConversions

@typeclass trait HasMetrics[A] {
  def getMetrics(a: A): MapTree[String, Metric]
}
object HasMetrics {
  implicit val intHasMetrics: HasMetrics[Int] =
    new HasMetrics[Int] {
      def getMetrics(i: Int): MapTree[String, Metric] =
        MapTree.leaf[String](Metric.int(i))
    }

  implicit val doubleHasMetrics: HasMetrics[Double] =
    new HasMetrics[Double] {
      def getMetrics(i: Double): MapTree[String, Metric] =
        MapTree.leaf[String](Metric.double(i))
    }

  implicit def mapHasMetrics[K: Show, V](implicit V: HasMetrics[V]): HasMetrics[Map[K, V]] =
    new HasMetrics[Map[K, V]] {
      def getMetrics(m: Map[K, V]): MapTree[String, Metric] =
        MapTree.Fork(
          m.map { case (k, v) =>
            k.show -> V.getMetrics(v)
          }
        )
    }
}
