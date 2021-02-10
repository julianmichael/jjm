package jjm.datasets.ontonotes

import org.scalatest._
import org.scalatest.prop._

import cats._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import java.nio.file.Paths

import freelog._
import freelog.implicits._

class OntonotesTests extends FunSuite with Matchers {

  implicit val ambLevel = LogLevel.Trace
  implicit def executionContext = scala.concurrent.ExecutionContext.global
  implicit val timer: Timer[IO] = IO.timer(executionContext)
  implicit val cs: ContextShift[IO] = IO.contextShift(executionContext)

  val location = Paths.get("data/conll-formatted-ontonotes-5.0")

  val service = new CoNLLFileSystemService(location)

  implicit val Log: EphemeralTreeLogger[IO, String] = freelog.loggers.TimingEphemeralTreeFansiLogger.create().unsafeRunSync

  var allPaths: List[CoNLLPath] = null

  test("Get all paths") {
    allPaths = service.getAllPaths.unsafeRunSync
    Log.info(s"${allPaths.size} files").unsafeRunSync
  }

  test("Get some files") {
    allPaths.take(10).infoBarTraverse("Getting some files") { path =>
      Log.info(s"$path") >> service.getFile(path)
    }.unsafeRunSync
  }

  test("Get all sentences") {
    val res = for {
      sentenceCounts <- allPaths.infoBarTraverse("Getting all sentences") { path =>
        Log.info(path.toString) >>
          service.getFile(path).map(f => Map(path.split -> f.sentences.size))
      }.map(_.combineAll)
      _ <- Log.info(s"sentence counts: $sentenceCounts")
      _ <- Log.info(s"Total: ${sentenceCounts.values.sum}")
    } yield ()
    res.unsafeRunSync
  }
}
