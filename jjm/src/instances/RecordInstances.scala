package jjm.instances

import jjm.metrics.HasMetrics
import jjm.metrics.MapTree
import jjm.metrics.Metric

import cats.Monoid
import cats.kernel.CommutativeMonoid
import cats.implicits._

import shapeless.HList
import shapeless.HNil
import shapeless.::
import shapeless.Witness
import shapeless.labelled.FieldType
import shapeless.labelled.field

trait RecordInstances extends LowPriorityRecordInstances {

  final implicit def hnilCommutativeMonoid: CommutativeMonoid[HNil] =
    new CommutativeMonoid[HNil] {
      def empty = HNil
      def combine(x: HNil, y: HNil) = HNil
    }

  final implicit def hconsRecordCommutativeMonoid[K, H: CommutativeMonoid, Tail <: HList : CommutativeMonoid]: CommutativeMonoid[FieldType[K, H] :: Tail] =
    new CommutativeMonoid[FieldType[K, H] :: Tail] {
      def empty = field[K](CommutativeMonoid[H].empty) :: CommutativeMonoid[Tail].empty
      def combine(x: FieldType[K, H] :: Tail, y: FieldType[K, H] :: Tail) =
        field[K]((x.head: H) |+| (y.head: H)) :: (x.tail |+| y.tail)
    }

  sealed trait RecordHasMetrics[R <: HList] {
    def apply(r: R): List[(String, MapTree[String, Metric])]
  }
  final object RecordHasMetrics {
    implicit def hnilHasMetrics: RecordHasMetrics[HNil] =
      new RecordHasMetrics[HNil] {
        def apply(r: HNil) = Nil
      }
    implicit def hconsHasMetrics[K <: String, V, Tail <: HList](
      implicit key: Witness.Aux[K],
      valueHasMetrics: HasMetrics[V],
      tailHasMetrics: RecordHasMetrics[Tail]
    ): RecordHasMetrics[FieldType[K, V] :: Tail] =
      new RecordHasMetrics[FieldType[K, V] :: Tail] {
        def apply(r: FieldType[K, V] :: Tail) = (key.value, valueHasMetrics.getMetrics(r.head)) :: tailHasMetrics(r.tail)
      }
  }
  final implicit def recordHasMetrics[R <: HList](implicit recordHasMetrics: RecordHasMetrics[R]): HasMetrics[R] = new HasMetrics[R] {
    def getMetrics(rec: R): MapTree[String, Metric] = {
      MapTree.fork(recordHasMetrics(rec).toMap)
    }
  }
}

trait LowPriorityRecordInstances {
  final implicit def hnilMonoid: Monoid[HNil] =
    new Monoid[HNil] {
      def empty = HNil
      def combine(x: HNil, y: HNil) = HNil
    }

  final implicit def hconsRecordMonoid[K, H: Monoid, Tail <: HList : Monoid]: Monoid[FieldType[K, H] :: Tail] =
    new Monoid[FieldType[K, H] :: Tail] {
      def empty = field[K](Monoid[H].empty) :: Monoid[Tail].empty
      def combine(x: FieldType[K, H] :: Tail, y: FieldType[K, H] :: Tail) =
        field[K]((x.head: H) |+| (y.head: H)) :: (x.tail |+| y.tail)
    }
}
