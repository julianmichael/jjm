package jjm.io

import java.nio.file.Path
import java.nio.file.Files

import cats.effect.IO
import cats.implicits._

class FileCached[A](
  path: Path,
  _read: Path => IO[A],
  write: (Path, A) => IO[Unit])(
  val compute: IO[A]
) {
  def read: IO[Option[A]] =
    IO(Files.exists(path)).ifM(
      _read(path).map(Some(_)),
      IO.pure(None)
    )

  def get: IO[A] =
    IO(Files.exists(path)).ifM(
      _read(path),
      compute.flatTap(x => write(path, x))
    )
}
object FileCached {
  def apply[A](
    path: Path,
    read: Path => IO[A],
    write: (Path, A) => IO[Unit])(
    compute: IO[A]
  ): FileCached[A] = new FileCached(path, read, write)(compute)

  def get[A](
    path: Path,
    read: Path => IO[A],
    write: (Path, A) => IO[Unit])(
    compute: IO[A]
  ): IO[A] = new FileCached(path, read, write)(compute).get
}
