package jjm.io

import cats.effect.IO
import cats.implicits._

// circumvent side-effect of ref creation
class Cell[A](create: IO[A]) {
  private[this] var value: Option[A] = None
  val get: IO[A] = IO(value).flatMap(
    _.map(IO.pure).getOrElse(
      create.flatTap(a => IO { value = Some(a) }): IO[A]
    )
  )
  val isPresent = IO(value.nonEmpty)
}
object Cell {
  def apply[A](create: IO[A]) = new Cell(create)
}
