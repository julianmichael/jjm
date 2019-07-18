package jjm.io

import jjm.Dot
import jjm.DotEncoder
import jjm.DotKleisli

// import cats.Functor
import cats.effect.Effect
import cats.implicits._

import _root_.io.circe.{Encoder, Decoder}

import org.http4s._

trait HttpUtilPlatformExtensions {
  def makeHttpPostServer[F[_]: Effect, Req <: Dot : Decoder : DotEncoder](
    backend: DotKleisli[F, Req]
  ): HttpRoutes[F] = {
    object dsl extends org.http4s.dsl.Http4sDsl[F]
    import dsl._
    import org.http4s.circe._
    implicit val entityDecoder = jsonOf[F, Req]
    HttpRoutes.of[F] {
      case rawReq @ POST -> Root => rawReq.as[Req].flatMap { req =>
        implicit val encoder = implicitly[DotEncoder[Req]].apply(req)
        implicit val entityEncoder = jsonEncoderOf[F, req.Out]
        backend(req).flatMap(Ok(_))
      }
    }
  }
}
