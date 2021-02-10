package jjm.datasets.conll08

import java.nio.file.Path

import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.Sync

import fs2.Stream

class CoNLL08FileSystemService(rootPath: Path) {
  def streamSentences[F[_]: Sync](split: CoNLL08Split)(
    implicit cs: ContextShift[F]): Stream[F, CoNLL08Sentence] = {
    val suffix = if(split.isTest) ".GOLD" else ""
    val filePath = rootPath.resolve(s"data/$split/$split.closed$suffix")
    Stream.resource(Blocker[F]).flatMap(blocker =>
      fs2.io.file.readAll[F](filePath, blocker, 4096)
        .through(fs2.text.utf8Decode)
        .through(fs2.text.lines)
        .through(CoNLL08Parsing.streamSentencesFromLines(split))
    )
  }
}
