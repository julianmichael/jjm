package jjm.datasets.ontonotes5

import cats.~>
import cats.Monad
import cats.implicits._
import cats.effect.IO

import java.nio.file.{Paths, Path, Files}

class CoNLLFileSystemService(location: Path) extends CoNLLService.MonadicCoNLLService[IO] {

  private[this] def getFileUnsafe(path: CoNLLPath): CoNLLFile = {
    import scala.collection.JavaConverters._
    CoNLLParsing.readFile(
      path,
      Files.lines(location.resolve(path.suffix)).iterator.asScala
    )
  }

  def getFile(path: CoNLLPath): IO[CoNLLFile] =
    IO(getFileUnsafe(path))

  def getAllPaths: IO[List[CoNLLPath]] = IO {
    def listFilePathsInRecursiveSubdirectories(rootPath: Path, file: java.io.File): List[String] =
      if(!file.isDirectory) List(rootPath.resolve(file.getName).toString)
      else file.listFiles.toList.flatMap(listFilePathsInRecursiveSubdirectories(rootPath.resolve(file.getName), _))
    new java.io.File(location.toString).listFiles.toList
      .flatMap(listFilePathsInRecursiveSubdirectories(Paths.get(""), _))
      .flatMap(CoNLLPath.fromPathSuffix)
  }
}
