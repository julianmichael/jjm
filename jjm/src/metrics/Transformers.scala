package jjm.metrics

import cats.Applicative
import cats.Functor
import cats.Eval
import cats.Monoid
import cats.Show
import cats.Traverse
import cats.implicits._

import monocle.function.Each

import HasMetrics.ops._

object Transformers {

  def map[InstanceA, InstanceB, MetricData](
    f: InstanceA => InstanceB)(
    metric: InstanceB => MetricData
  ): (InstanceA => MetricData) = (a: InstanceA) => {
    metric(f(a))
  }

  def bucket[Instance, MetricData](
    bucketers: Map[String, Instance => String])(
    metric: Instance => MetricData
  ): (Instance => Bucketed[MetricData]) = (instance: Instance) => {
    val bucket = bucketers.map { case (key, bucketer) => key -> bucketer(instance) }
    Bucketed(Map(bucket -> metric(instance)))
  }

  def choose[Param, Instance, MetricData](
    params: List[Param])(
    metric: Param => Instance => MetricData
  ): (Instance => Chosen[Param, MetricData]) = (instance: Instance) => {
    Chosen(params.map(p => p -> metric(p)(instance)).toMap)
  }

  // def choose[Param, Instance, MetricData](
  //   choices: (Param, Instance => MetricData)*
  // ): (Instance => Chosen[Param, MetricData]) = (instance: Instance) => {
  //   Chosen(choices.map { case (p, metric) => p -> metric(instance) }.toMap)
  // }

  def split[BigInstance, SmallInstance, MetricData: Monoid](
    splittingFn: BigInstance => List[SmallInstance])(
    metric: SmallInstance => MetricData
  ) = (bigInstance: BigInstance) => {
    splittingFn(bigInstance).foldMap(metric)
  }

  import shapeless.{HList, ::, HNil}
  import shapeless.Generic
  import shapeless.Witness
  import shapeless.labelled.field
  import shapeless.labelled.FieldType

  sealed trait InstanceMapper[Instance, Fn <: HList] {
    type Out <: HList
    def apply(fn: Fn): Instance => Out
  }
  object InstanceMapper {
    type Aux[I, Fn <: HList, Out0 <: HList] = InstanceMapper[I, Fn] { type Out = Out0 }

    implicit def hnilInstanceMapper[I]: Aux[I, HNil, HNil] =
      new InstanceMapper[I, HNil] {
        type Out = HNil
        def apply(fn: HNil) = (i: I) => HNil
      }

    implicit def hconsInstanceMapper[I, O, K, Tail <: HList, TailOut <: HList](
      implicit tailMapper: Aux[I, Tail, TailOut]
    ): Aux[I, FieldType[K, I => O] :: Tail, FieldType[K, O] :: TailOut] =
      new InstanceMapper[I, FieldType[K, I => O] :: Tail] {
        type Out = FieldType[K, O] :: TailOut
        def apply(fn: FieldType[K, I => O] :: Tail) = (i: I) => field[K](fn.head(i)) :: tailMapper(fn.tail)(i)
    }
  }

  def hchoose[Instance, P <: Product, Mappers <: HList, Rec <: HList](
    choices: P)(
    implicit gen: Generic.Aux[P, Mappers],
    instanceMapper: InstanceMapper.Aux[Instance, Mappers, Rec]
  ): (Instance => Rec) = (instance: Instance) => {
    instanceMapper(gen.to(choices))(instance)
  }
}
