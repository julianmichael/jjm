package jjm

import cats.Id

sealed trait DotPair[F[_], A <: Dot] extends Product with Serializable {
  val fst: A
  val snd: F[fst.Out]
}

object DotPair {
  private[this] case class DotPairImpl[F[_], A <: Dot, B](
    val fst: A { type Out = B },
    val snd: F[B]
  ) extends DotPair[F, A] {
    override def toString = s"DotPair($fst, $snd)"
  }

  // def apply[F[_], A <: Dot](fst: A)(snd: F[fst.Out]): DotPair[F, A] =
  //   DotPairImpl[A, F[fst.Out]](fst, snd)
  def unapply[F[_], A <: Dot](dp: DotPair[F, A]): Option[(A, F[dp.fst.Out])] =
    Some(dp.fst -> dp.snd)

  def apply[F[_]] = new ApplyBuilder[F]
  class ApplyBuilder[F[_]] {
    def apply[A <: Dot](fst: A)(snd: F[fst.Out]): DotPair[F, A] = DotPairImpl[F, A, fst.Out](fst, snd)
  }

  val unit = apply[Id](jjm.DotUnit)(())

  import io.circe.Encoder
  import io.circe.Decoder
  import io.circe.syntax._

  implicit def dotPairEncoder[F[_], A <: Dot](
    implicit fstEncoder: Encoder[A],
    sndDotEncoder: DotKleisli[λ[B => Encoder[F[B]]], A]
  ): Encoder[DotPair[F, A]] = Encoder.instance[DotPair[F, A]] { pair =>
    implicit val sndEncoder = sndDotEncoder(pair.fst)
    List(pair.fst.asJson, pair.snd.asJson).asJson
  }

  // TODO
  // implicit def dotMapDecoder[F[_], A <: Dot](
  //   implicit fstDecoder: Decoder[A],
  //   sndDotDecoder: DotKleisli[λ[B => Decoder[F[B]]], A]
  // ): Decoder[DotPair[F, A]] = new Decoder[DotPair[F, A]] {
  //   final def apply(c: HCursor): Decoder.Result[DotPair[F, A]] = {
  //     ???
  //   }
  // }

  // TODO cats instances
  // monocle instances probably aren't possible...? maybe with singleton types? TODO try it out
}
