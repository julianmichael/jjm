package jjm.io

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Resource}

import _root_.io.circe.jawn
import _root_.io.circe.{Encoder, Decoder}
import _root_.io.circe.Printer

import fs2.Stream

import java.nio.file.{Path => NIOPath}
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService
import cats.effect.Blocker

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

  def useCompression(path: NIOPath, setting: CompressionSetting): Boolean = setting match {
    case CompressionSetting.Compressed => true
    case CompressionSetting.Uncompressed => false
    case CompressionSetting.Auto => path.toString.endsWith(".gz")
  }

  val bufferNumBytes = 4 * 4096

  def streamLines(
    path: NIOPath, blocker: Blocker, compression: CompressionSetting = CompressionSetting.Auto)(
    implicit cs: ContextShift[IO]
  ): Stream[IO, String] = {
    val fileBytes = fs2.io.file.readAll[IO](path, blocker, bufferNumBytes)
    val textBytes = if(useCompression(path, compression)) {
      fileBytes.through(fs2.compression.gunzip(bufferNumBytes)).flatMap(_.content)
    } else fileBytes
    textBytes
      .through(fs2.text.utf8Decode)
      .through(fs2.text.lines)
  }

  def streamJsonLines[A](
    path: NIOPath, blocker: Blocker, compression: CompressionSetting = CompressionSetting.Auto)(
    implicit cs: ContextShift[IO], decoder: Decoder[A]
  ): Stream[IO, A] = {
    streamLines(path, blocker, compression)
      .filter(_.nonEmpty)
      .flatMap(line =>
        Stream.fromEither[IO](
          jawn.decode[A](line)
            .left.map(e => new RuntimeException(s"${e.show}\n$line"))
        )
      )
  }

  def readLines(
    path: NIOPath, compression: CompressionSetting = CompressionSetting.Auto)(
    implicit cs: ContextShift[IO]
  ): Stream[IO, String] = {
    Stream.resource(Blocker[IO]).flatMap { blocker =>
      streamLines(path, blocker, compression)
    }
  }

  def readJsonLines[A](
    path: NIOPath, compression: CompressionSetting = CompressionSetting.Auto)(
    implicit cs: ContextShift[IO], decoder: Decoder[A]
  ): Stream[IO, A] = {
    Stream.resource(Blocker[IO]).flatMap { blocker =>
      streamJsonLines(path, blocker, compression)
    }
  }

  def readJson[A: Decoder](path: NIOPath): IO[A] = {
    IO(
      _root_.io.circe.jawn.decodeFile[A](new java.io.File(path.toString)) match {
        case Right(a) => a
        case Left(e) => throw new RuntimeException(s"${e.show}")
      }
    )
  }

  def writeJson[A: Encoder](path: NIOPath, printer: Printer = io.circe.Printer.noSpaces)(a: A): IO[Unit] = {
    import _root_.io.circe.syntax._
    IO(Option(path.getParent).foreach(java.nio.file.Files.createDirectories(_))) >>
      IO(java.nio.file.Files.write(path, printer.print(a.asJson).getBytes("UTF-8")))
  }

  def writeString(path: NIOPath)(a: String): IO[Unit] = {
    IO(java.nio.file.Files.write(path, a.getBytes("UTF-8")))
  }

  def readString(path: NIOPath): IO[String] = {
    import scala.collection.JavaConverters._
    IO(java.nio.file.Files.lines(path).iterator.asScala.mkString("\n"))
  }

  def writeJsonLines[A](path: NIOPath, printer: Printer = io.circe.Printer.noSpaces, compression: CompressionSetting = CompressionSetting.Auto)(as: Seq[A])(
    implicit cs: ContextShift[IO], encoder: Encoder[A]
  ): IO[Unit] = {
    import _root_.io.circe.syntax._
    IO(Option(path.getParent).foreach(java.nio.file.Files.createDirectories(_))) >>
      Stream.resource(Blocker[IO]).flatMap { blocker =>
        val textOut = Stream.emits[IO, A](as)
          .map(a => printer.print(a.asJson))
          .intersperse("\n")
          .through(fs2.text.utf8Encode)
        val compressedTextOut = if(useCompression(path, compression)) {
          textOut.through(fs2.compression.gzip(bufferNumBytes))
        } else textOut
        compressedTextOut.through(fs2.io.file.writeAll(path, blocker))
      }.compile.drain
  }

  def writeJsonLinesStreaming[A](path: NIOPath, printer: Printer, compression: CompressionSetting = CompressionSetting.Auto)(as: Stream[IO, A])(
    implicit cs: ContextShift[IO], encoder: Encoder[A]
  ): IO[Unit] = {
    import _root_.io.circe.syntax._
    IO(Option(path.getParent).foreach(java.nio.file.Files.createDirectories(_))) >>
      Stream.resource(Blocker[IO]).flatMap { blocker =>
        val textOut = as
          .map(a => printer.print(a.asJson))
          .intersperse("\n")
          .through(fs2.text.utf8Encode)
        val compressedTextOut = if(useCompression(path, compression)) {
          textOut.through(fs2.compression.gzip(bufferNumBytes))
        } else textOut
        compressedTextOut.through(fs2.io.file.writeAll(path, blocker))
      }.compile.drain
  }

  def writeLines[A](path: NIOPath, getString: A => String, compression: CompressionSetting = CompressionSetting.Auto)(as: Seq[A])(
    implicit cs: ContextShift[IO], encoder: Encoder[A]
  ): IO[Unit] = {
    IO(Option(path.getParent).foreach(java.nio.file.Files.createDirectories(_))) >>
      Stream.resource(Blocker[IO]).flatMap { blocker =>
        val textOut = Stream.emits[IO, A](as)
          .map(getString)
          .intersperse("\n")
          .through(fs2.text.utf8Encode)
        val compressedTextOut = if(useCompression(path, compression)) {
          textOut.through(fs2.compression.gzip(bufferNumBytes))
        } else textOut
        compressedTextOut.through(fs2.io.file.writeAll(path, blocker))
      }.compile.drain
  }
}
