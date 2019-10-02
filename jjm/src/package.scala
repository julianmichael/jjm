import io.circe.Decoder
import io.circe.Encoder

import cats.Id
import cats.~>

package object jjm {

  type Dot = Any { type Out }

  // TODO maybe there's a better name for this than DotF?
  type DotF[A] = { type Aux[B] = A { type Out = B } }

  type DotFunction[A <: Dot] = DotKleisli[Id, A]

  type DotFunctionK[F[_], A <: Dot] = DotF[A]#Aux ~> F

  sealed trait DotUnit extends Product with Serializable {
    final type Out = Unit
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
