package jjm.datasets.ptb2

import java.nio.file.{Files, Path, Paths}

import scala.concurrent.ExecutionContext

import cats.effect.ContextShift
import cats.effect.Sync
import cats.implicits._

import fs2.Stream
import cats.effect.Blocker

class PTB2FileSystemService(location: Path) {
  private[this] val wsjAnnotationPath =
    location.resolve(Paths.get("combined/wsj"))

  // TODO replace with fs2 stuff after update
  private def listFiles[F[_]: Sync](path: Path): F[List[Path]] = Sync[F].delay {
    import scala.collection.JavaConverters._
    new java.io.File(path.toString).listFiles.iterator
      .map(f => path.resolve(f.getName))
      .toList
  }

  def streamFiles[F[_]: Sync](
    implicit cs: ContextShift[F]
  ): Stream[F, PTB2File] = {
    Stream.resource(Blocker[F]).flatMap { blocker =>
      Stream.emits[F, Int](0 to 24) >>= { section =>
        val sectionPath = wsjAnnotationPath.resolve(f"$section%02d")
        Stream.eval(listFiles(sectionPath))
          .flatMap(Stream.emits[F, Path])
          .evalMap { path =>
            val fileName = path.getFileName.toString
            PTB2Parsing.readFile[F, F](
              PTB2FilePath(section, fileName),
              fs2.io.file.readAll[F](path, blocker, 4096)
                .through(fs2.text.utf8Decode)
                .through(fs2.text.lines)
            )
          }
      }
    }
  }
}
