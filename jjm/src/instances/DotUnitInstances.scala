package jjm.instances

import jjm.DotUnit
import jjm.DotEncoder
import jjm.DotDecoder

import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json

trait DotUnitInstances {
  implicit val dotUnitDecoder = Decoder.instance((_: HCursor) => Right(DotUnit))
  implicit val dotUnitEncoder = Encoder.instance((_: DotUnit) => Json.obj())

  implicit val dotUnitDotEncoder = new DotEncoder[DotUnit] {
    def apply(du: DotUnit) = implicitly[Encoder[Unit]]
  }
  implicit val dotUnitDotDecoder = new DotDecoder[DotUnit] {
    def apply(du: DotUnit) = implicitly[Decoder[Unit]]
  }
}
