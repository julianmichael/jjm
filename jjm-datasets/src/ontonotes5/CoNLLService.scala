package jjm.datasets.ontonotes5

// import cats.free.Free
import cats.~>
import cats.Monad
import cats.implicits._

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

import jjm.DotKleisli

// trait CoNLLService[M[_]] {

//   protected implicit def monad: Monad[M]

//   final def interpreter: (CoNLLServiceRequestA ~> M) = new (CoNLLServiceRequestA ~> M) {
//     import CoNLLServiceRequestA._
//     def apply[A](op: CoNLLServiceRequestA[A]): M[A] = op match {
//       case GetFile(path) => getFile(path)
//       case GetAllPaths => getAllPaths
//       case GetSentence(sentencePath) => getSentence(sentencePath)
//       case GetAllSentencePaths => getAllSentencePaths
//     }
//   }

//   final def interpretThrough[G[_]: Monad](transform: M ~> G): CoNLLService[G] =
//     new CoNLLService.CompoundCoNLLService(this, transform)
// }

// object CoNLLService {
//   private class CompoundCoNLLService[M[_], G[_]](
//     base: CoNLLService[M],
//     transform: M ~> G)(
//     implicit M: Monad[M],
//     G: Monad[G]
//   ) extends CoNLLService[G] {
//     override protected implicit val monad = G

//     def getFile(path: CoNLLPath): G[CoNLLFile] =
//       transform(base.getFile(path))

//     def getAllPaths: G[List[CoNLLPath]] =
//       transform(base.getAllPaths)

//     override def getSentence(path: CoNLLSentencePath): G[CoNLLSentence] =
//       transform(base.getSentence(path))

//     override def getAllSentencePaths: G[List[CoNLLSentencePath]] =
//       transform(base.getAllSentencePaths)
//   }
// }

// sealed trait CoNLLServiceRequestA[A]
// object CoNLLServiceRequestA {
//   case class GetFile(path: CoNLLPath) extends CoNLLServiceRequestA[CoNLLFile]
//   case object GetAllPaths extends CoNLLServiceRequestA[List[CoNLLPath]]
//   case class GetSentence(sentencePath: CoNLLSentencePath) extends CoNLLServiceRequestA[CoNLLSentence]
//   case object GetAllSentencePaths extends CoNLLServiceRequestA[List[CoNLLSentencePath]]
// }

// object FreeCoNLLService extends CoNLLService[Free[CoNLLServiceRequestA, ?]] {

//   type CoNLLServiceRequest[A] = Free[CoNLLServiceRequestA, A]

//   protected implicit override val monad: Monad[CoNLLServiceRequest] =
//     implicitly[Monad[CoNLLServiceRequest]]

//   def getFile(path: CoNLLPath): CoNLLServiceRequest[CoNLLFile] =
//     Free.liftF[CoNLLServiceRequestA, CoNLLFile](CoNLLServiceRequestA.GetFile(path))

//   def getAllPaths: CoNLLServiceRequest[List[CoNLLPath]] =
//     Free.liftF[CoNLLServiceRequestA, List[CoNLLPath]](CoNLLServiceRequestA.GetAllPaths)

//   override def getSentence(sentencePath: CoNLLSentencePath): CoNLLServiceRequest[CoNLLSentence] =
//     Free.liftF[CoNLLServiceRequestA, CoNLLSentence](CoNLLServiceRequestA.GetSentence(sentencePath))

//   override def getAllSentencePaths: CoNLLServiceRequest[List[CoNLLSentencePath]] =
//     Free.liftF[CoNLLServiceRequestA, List[CoNLLSentencePath]](CoNLLServiceRequestA.GetAllSentencePaths)
// }


trait CoNLLService[F[_]] extends DotKleisli[F, CoNLLService.Request] {
  import CoNLLService._

  def getFile(path: CoNLLPath): F[CoNLLFile]

  def getAllPaths: F[List[CoNLLPath]]

  def getSentence(path: CoNLLSentencePath): F[CoNLLSentence]

  def getAllSentencePaths: F[List[CoNLLSentencePath]]

  // TODO why doesn't this work eh. i swear it worked elsewhere
  override def apply(req: Request): F[req.Out] = req match {
    case GetFile(path) => getFile(path).asInstanceOf[F[req.Out]]
    case GetAllPaths => getAllPaths.asInstanceOf[F[req.Out]]
    case GetSentence(path) => getSentence(path).asInstanceOf[F[req.Out]]
    case GetAllSentencePaths => getAllSentencePaths.asInstanceOf[F[req.Out]]
  }
  // override def apply(req: Request): F[req.Out] = _apply[req.Out](req)
  // private[this] def _apply[A](req: Request { type Out = A }): F[A] = req match {
  //   case GetFile(path) => getFile(path)
  //   case GetAllPaths => getAllPaths
  //   case GetSentence(path) => getSentence(path)
  //   case GetAllSentencePaths => getAllSentencePaths
  // }
}
object CoNLLService {

  abstract class MonadicCoNLLService[F[_]: Monad] extends CoNLLService[F] {
    def getSentence(path: CoNLLSentencePath): F[CoNLLSentence] =
      getFile(path.filePath).map(_.sentences(path.sentenceNum))

    def getAllSentencePaths: F[List[CoNLLSentencePath]] = for {
      paths <- getAllPaths
      files <- paths.map(getFile).sequence
    } yield files.flatMap(_.sentences.map(_.path))
  }

  // TODO json codecs
  sealed trait Request { type Out }
  case class GetFile(path: CoNLLPath) extends Request { type Out = CoNLLFile }
  case object GetAllPaths extends Request { type Out = List[CoNLLPath] }
  case class GetSentence(path: CoNLLSentencePath) extends Request { type Out = CoNLLSentence }
  case object GetAllSentencePaths extends Request { type Out = List[CoNLLSentencePath] }

  object Request {
    // TODO implement if necessary
    // implicit val conllServiceRequestDotEncoder: DotEncoder[Request] = new DotEncoder[Request] {
    //   def apply(req: Request): Encoder[req.Out] = _apply[req.Out](req)
    //   private[this] def _apply[A](req: Request { type Out = A }): Encoder[A] = req match {
    //     case GetFile(_) => implicitly[Encoder[CoNLLFile]]
    //     case GetAllPaths => implicitly[Encoder[List[CoNLLPath]]]
    //     case GetSentence(_) => implicitly[Encoder[CoNLLSentence]]
    //     case GetAllSentencePaths => implicitly[Encoder[List[CoNLLSentencePath]]]
    //   }
    // }
    // implicit val conllServiceRequestDotDecoder: DotDecoder[Request] = new DotDecoder[Request] {
    //   def apply(req: Request): Decoder[req.Out] = _apply[req.Out](req)
    //   private[this] def _apply[A](req: Request { type Out = A }): Decoder[A] = req match {
    //     case GetFile(_) => implicitly[Decoder[CoNLLFile]]
    //     case GetAllPaths => implicitly[Decoder[List[CoNLLPath]]]
    //     case GetSentence(_) => implicitly[Decoder[CoNLLSentence]]
    //     case GetAllSentencePaths => implicitly[Decoder[List[CoNLLSentencePath]]]
    //   }
    // }
  }
}
