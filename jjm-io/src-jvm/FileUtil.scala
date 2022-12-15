package jjm.io

import cats.implicits._
import cats.data.NonEmptyList

import cats.effect._

import _root_.io.circe.jawn
import _root_.io.circe.{Encoder, Decoder}
import _root_.io.circe.Printer

import fs2.Stream

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text

object FileUtil {

  def sendToClipboard(s: String): IO[Unit] = IO {
    import java.awt._;
    import java.awt.datatransfer._;
    import java.io._;
    val selection = new StringSelection(s)
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    clipboard.setContents(selection, selection)
  }

  sealed trait CompressionSetting
  object CompressionSetting {
    case object Compressed extends CompressionSetting
    case object Uncompressed extends CompressionSetting
    case object Auto extends CompressionSetting
  }

  def useCompression(path: Path, setting: CompressionSetting): Boolean = setting match {
    case CompressionSetting.Compressed => true
    case CompressionSetting.Uncompressed => false
    case CompressionSetting.Auto => path.toString.endsWith(".gz")
  }

  val bufferNumBytes = 4 * 4096

  def streamLines(
    path: Path, compression: CompressionSetting = CompressionSetting.Auto
  ): Stream[IO, String] = {
    val fileBytes = Files[IO].readAll(path)
    val textBytes = if(useCompression(path, compression)) {
      fileBytes.through(fs2.compression.Compression[IO].gunzip(bufferNumBytes)).flatMap(_.content)
    } else fileBytes
    textBytes
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
  }

  def streamJsonLines[A](
    path: Path, compression: CompressionSetting = CompressionSetting.Auto)(
    implicit decoder: Decoder[A]
  ): Stream[IO, A] = {
    streamLines(path, compression)
      .filter(_.nonEmpty)
      .flatMap(line =>
        Stream.fromEither[IO](
          jawn.decode[A](line)
            .left.map(e => new RuntimeException(s"${e.show}\n$line"))
        )
      )
  }

  def readLines(
    path: Path, compression: CompressionSetting = CompressionSetting.Auto
  ): Stream[IO, String] = {
    streamLines(path, compression)
  }

  def readJsonLines[A](
    path: Path, compression: CompressionSetting = CompressionSetting.Auto)(
    implicit decoder: Decoder[A]
  ): Stream[IO, A] = {
    streamJsonLines(path, compression)
  }

  def readJson[A: Decoder](path: Path): IO[A] = {
    IO(
      _root_.io.circe.jawn.decodeFile[A](new java.io.File(path.toString)) match {
        case Right(a) => a
        case Left(e) => throw new RuntimeException(s"${e.show}")
      }
    )
  }

  def writeJson[A: Encoder](path: Path, printer: Printer = io.circe.Printer.noSpaces)(a: A): Any = {
    import _root_.io.circe.syntax._
    writeString(path)(printer.print(a.asJson))
  }

  def writeString(path: Path)(a: String): IO[Unit] = {
    path.parent.traverse_(Files[IO].createDirectories(_)) >>
      Stream.emit[IO, String](a)
        .through(text.utf8.encode)
        .through(Files[IO].writeAll(path))
        .compile.drain
  }

  def readString(path: Path): IO[String] = {
    Files[IO].readUtf8(path).compile.foldMonoid
  }

  def writeJsonLines[A](path: Path, printer: Printer = io.circe.Printer.noSpaces, compression: CompressionSetting = CompressionSetting.Auto)(as: Seq[A])(
    implicit encoder: Encoder[A]
  ): IO[Unit] = {
    import _root_.io.circe.syntax._
    path.parent.traverse_(Files[IO].createDirectories(_)) >> {
      val textOut = Stream.emits[IO, A](as)
        .map(a => printer.print(a.asJson))
        .intersperse("\n")
        .through(fs2.text.utf8.encode)
      val compressedTextOut = if(useCompression(path, compression)) {
        textOut.through(fs2.compression.Compression[IO].gzip(bufferNumBytes))
      } else textOut
      compressedTextOut.through(Files[IO].writeAll(path)).compile.drain
    }
  }

  def writeJsonLinesStreaming[A](path: Path, printer: Printer, compression: CompressionSetting = CompressionSetting.Auto)(as: Stream[IO, A])(
    implicit encoder: Encoder[A]
  ): IO[Unit] = {
    import _root_.io.circe.syntax._
    path.parent.traverse_(Files[IO].createDirectories(_)) >> {
      val textOut = as
        .map(a => printer.print(a.asJson))
        .intersperse("\n")
        .through(fs2.text.utf8.encode)
      val compressedTextOut = if(useCompression(path, compression)) {
        textOut.through(fs2.compression.Compression[IO].gzip(bufferNumBytes))
      } else textOut
      compressedTextOut.through(Files[IO].writeAll(path)).compile.drain
    }
  }

  def writeLines[A](path: Path, getString: A => String, compression: CompressionSetting = CompressionSetting.Auto)(as: Seq[A])(
    implicit encoder: Encoder[A]
  ): IO[Unit] = {
    path.parent.traverse_(Files[IO].createDirectories(_)) >> {
      val textOut = Stream.emits[IO, A](as)
        .map(getString)
        .intersperse("\n")
        .through(fs2.text.utf8.encode)
      val compressedTextOut = if(useCompression(path, compression)) {
        textOut.through(fs2.compression.Compression[IO].gzip(bufferNumBytes))
      } else textOut
      compressedTextOut.through(Files[IO].writeAll(path)).compile.drain
    }
  }
}
