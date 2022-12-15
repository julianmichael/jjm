package jjm.io

import jjm.Dot
import jjm.DotEncoder
import jjm.DotDecoder
import jjm.DotKleisli

import cats.implicits._
import cats.MonadError
import cats.effect.Concurrent

import _root_.io.circe.{Encoder, Decoder}
import _root_.io.circe.Json

import org.http4s._
import org.http4s.client.Client

trait HttpUtilPlatformExtensions {
  def makeHttpPostClient[F[_]: Concurrent, Req <: Dot : Encoder : DotDecoder](
    client: Client[F], endpoint: Uri
  ): DotKleisli[F, Req] = {
    object dsl extends org.http4s.dsl.Http4sDsl[F]
    import dsl._
    import org.http4s.circe._
    implicit val entityEncoder = jsonEncoderOf[F, Req]
    new DotKleisli[F, Req] {
      EntityDecoder
      def apply(req: Req): F[req.Out] = client
        .fetchAs[Json](Request[F](method = Method.POST, uri = endpoint).withEntity(req))
        .map(implicitly[DotDecoder[Req]].apply(req).decodeJson(_))
        .map(_.leftMap(new RuntimeException(_)))
        .flatMap(Concurrent[F].fromEither[req.Out](_))
    }
  }

  def makeHttpPostServer[F[_]: Concurrent, Req <: Dot : Decoder : DotEncoder](
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
