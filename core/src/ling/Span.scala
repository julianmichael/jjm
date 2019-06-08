package jjm.ling

sealed trait Span {
  def begin: Int
  def end: Int
  final def length = end - begin + 1
  final def contains(i: Int) = begin <= i && i <= end
}

object Span {
  private[this] case class SpanImpl(override val begin: Int, override val end: Int) extends Span {
    override def toString = s"Span($begin, $end)"
  }
  def apply(x: Int, y: Int): Span = SpanImpl(math.min(x, y), math.max(x, y))
  def unapply(cs: Span): Option[(Int, Int)] = SpanImpl.unapply(cs.asInstanceOf[SpanImpl])

  // no Lenses because they would not actually be lawful due to the ordering constraint on begin/end

  import io.circe.Encoder
  import io.circe.Decoder
  import io.circe.syntax._

  implicit val spanEncoder: Encoder[Span] = Encoder.instance[Span] { span =>
    List(span.begin, span.end).asJson
  }

  implicit val spanDecoder: Decoder[Span] = Decoder.instance { c =>
    c.as[List[Int]].map(l => Span(l(0), l(1)))
  }
}
