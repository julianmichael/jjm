package jjm.io

import jjm.Dot
import jjm.DotKleisli
import jjm.DotDecoder
import jjm.implicits._

import cats.effect.Effect

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

import _root_.io.circe.{Encoder, Decoder}
import _root_.io.circe.syntax._

trait HttpUtilPlatformExtensions {
  // TODO something better than () => Future
  // TODO handle errors more gracefully; this would be better with a bifunctor IO or Task
  def makeHttpPostClient[Req <: Dot : Encoder : DotDecoder](
    endpoint: String // TODO url
  )(implicit ec: ExecutionContext): DotKleisli[
    Lambda[A => () => Future[A]], Req
  ] = new DotKleisli[Lambda[A => () => Future[A]], Req] {
    def apply(req: Req): () => Future[req.Out] = () => {
      org.scalajs.dom.ext.Ajax.post(url = endpoint, data = req.asJson.noSpaces)
        .map(resp => io.circe.parser.decode(resp.responseText)(implicitly[DotDecoder[Req]].apply(req)))
        .flatMap {
          case Right(res) => Future.successful(res)
          case Left(fail) => Future.failed(new RuntimeException(fail))
        }
    }
  }
}
