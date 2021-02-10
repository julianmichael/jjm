package jjm.datasets.propbank3

import java.nio.file.Path
import java.nio.file.Paths

import scala.concurrent.ExecutionContext

import cats.effect.ContextShift
import cats.effect.Sync
import cats.implicits._

import fs2.Stream
import cats.effect.Blocker

// TODO: handle more of the dataset. all we need now is wsj though
class PropBank3FileSystemService(location: Path) {
  private[this] val dataPath =
    location.resolve(Paths.get("data/ontonotes/nw/wsj"))

  def listFiles(path: Path) = {
    import scala.collection.JavaConverters._
    new java.io.File(path.toString).listFiles
      .iterator
      .map(f => path.resolve(f.getName))
  }

  def streamSentencesFromProps[F[_]: Sync](
    implicit cs: ContextShift[F]
  ): Stream[F, PropBank3Sentence] = {
    Stream.resource(Blocker[F]).flatMap { blocker =>
      Stream.fromIterator(listFiles(dataPath))
        .flatMap(path => Stream.fromIterator(listFiles(path)))
        .filter(_.getFileName.toString.endsWith(".prop"))
        .flatMap(path =>
          Stream.eval(
              fs2.io.file.readAll[F](path, blocker, 4096)
                .through(fs2.text.utf8Decode)
                .through(fs2.text.lines)
                .through(PropBank3Parsing.streamPredArgStructuresFromProps[F])
                .compile.toList.map(
                  _.groupBy(_._1).toList.map { case (id, pairs) =>
                    PropBank3Sentence(id, pairs.map(_._2))
                  }
                ).map(Stream.emits(_))
          ).flatten
        )
    }
  }

  def streamSentencesFromCoNLLSkels[F[_]: Sync](
    implicit cs: ContextShift[F]
  ): Stream[F, PropBank3SentenceCoNLLStyle] = {
    Stream.resource(Blocker[F]).flatMap { blocker =>
      Stream.fromIterator(listFiles(dataPath))
        .flatMap(path => Stream.fromIterator(listFiles(path)))
        .filter(_.getFileName.toString.endsWith(".gold_skel"))
        .flatMap(path =>
          fs2.io.file.readAll[F](path, blocker, 4096)
            .through(fs2.text.utf8Decode)
            .through(fs2.text.lines)
            .through(PropBank3Parsing.streamSentencesFromCoNLLSkels[F])
        )
    }
  }
}
