package jjm.io

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

// circumvent side-effect of ref creation
class Cell[A](create: IO[A]) {
  private[this] val value: Ref[IO, Option[A]] = {
    Ref[IO].of[Option[A]](None).unsafeRunSync()
  }
  val get: IO[A] = value.get.flatMap(innerValue =>
    innerValue.map(IO.pure).getOrElse(
      create.flatTap(a => value.set(Some(a)))
    )
  )
  val isPresent = value.get.map(_.nonEmpty)
}
object Cell {
  def apply[A](create: IO[A]) = new Cell(create)
}
