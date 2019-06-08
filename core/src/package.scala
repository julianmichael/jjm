import io.circe.Decoder
import io.circe.Encoder

import cats.~>

package object jjm {

  protected[jjm] type Id[A] = A // to use here but not clash with cats

  type Dot = Any { type Arg }

  // TODO maybe there's a better name for this than DotF?
  type DotF[A] = { type Aux[B] = A { type Arg = B } }

  type DotFunction[A <: Dot] = DotKleisli[Id, A]

  sealed trait DotUnit extends Product with Serializable {
    final type Arg = Unit
  }
  private[this] case class DotUnitImpl() extends DotUnit {
    override def toString = "DotUnit"
  }
  val DotUnit: DotUnit = DotUnitImpl()

  val LowerCaseString: LowerCaseStringCapsule = LowerCaseStringImpl
  type LowerCaseString = LowerCaseString.Type

  // circe stuff

  type DotEncoder[A <: Dot] = DotKleisli[Encoder, A]
  type DotDecoder[A <: Dot] = DotKleisli[Decoder, A]

  type DependentEncoder[F[_], G[_]] = F ~> λ[A => Encoder[G[A]]]
  type DependentDecoder[F[_], G[_]] = F ~> λ[A => Decoder[G[A]]]

}
