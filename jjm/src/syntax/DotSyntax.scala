package jjm.syntax

import jjm.Dot
import jjm.DotKleisli

trait DotSyntax {
  implicit class RichDot[A <: Dot](val a: A) {
    def run[F[_]](implicit dk: DotKleisli[F, A]): F[a.Out] = dk(a)
  }
}
