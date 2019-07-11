package jjm.ling

import cats.Order
import cats.Semigroup
import cats.Show
import cats.TraverseFilter
import cats.implicits._

import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax._

// generic Span should be used only for interfaces which don't care which type it is;
// data should not generally be passed around in this form.
// hence why we don't have any instances except Show.
sealed trait Span {
  def length: Int
  def contains(i: Int): Boolean
  def toInclusive: ISpan
  def toExclusive: ESpan
  def getSlice[F[_]: TraverseFilter, A](sequence: F[A]): F[A] = {
    // TODO make this more efficient
    val traverse = implicitly[TraverseFilter[F]].traverse
    traverse.map(
      traverse.zipWithIndex(sequence)
        .filter(p => contains(p._2)
        )
    )(_._1)
  }
}
object Span {
  // use ISpan and ESpan apply methods instead, unless you want to restrict imports or something.
  def inclusive(begin: Int, end: Int) = ISpan(begin, end)
  def exclusive(begin: Int, end: Int) = ESpan(begin, end)

  // will do the same as a subclass Show anyway
  implicit val spanShow: Show[Span] = Show.show[Span](_.toString)
}

// index-inclusive span
sealed trait ISpan extends Span {
  def begin: Int
  def end: Int
  final def length = end - begin + 1
  final def contains(i: Int) = begin <= i && i <= end
  final def toInclusive: ISpan = this
  final def toExclusive: ESpan = ESpan(begin, end + 1)
}

object ISpan {
  private[this] case class ISpanImpl(override val begin: Int, override val end: Int) extends ISpan {
    override def toString = s"[$begin, $end]"
  }
  def apply(x: Int, y: Int): ISpan = ISpanImpl(math.min(x, y), math.max(x, y))
  def unapply(cs: ISpan): Option[(Int, Int)] = ISpanImpl.unapply(cs.asInstanceOf[ISpanImpl])

  // no Lenses because they would not actually be lawful due to the ordering constraint on begin/end

  implicit val ispanEncoder: Encoder[ISpan] = Encoder.instance[ISpan] { span =>
    List(span.begin, span.end).asJson
  }

  implicit val ispanDecoder: Decoder[ISpan] = Decoder.instance { c =>
    c.as[List[Int]].map(l => ISpan(l(0), l(1)))
  }

  implicit val ispanShow: Show[ISpan] = Show.show[ISpan](_.toString)

  implicit val ispanOrder: Order[ISpan] = Order.whenEqual(
    Order.by[ISpan, Int](_.begin),
    Order.by[ISpan, Int](_.end),
  )

  implicit val ispanSemigroup: Semigroup[ISpan] = new Semigroup[ISpan] {
    def combine(x: ISpan, y: ISpan) = ISpan(math.min(x.begin, y.begin), math.max(x.end, y.end))
  }
}

// end index-exclusive span
sealed trait ESpan extends Span {
  def begin: Int
  def end: Int
  final def length = end - begin
  final def contains(i: Int) = begin <= i && i < end
  final def toInclusive: ISpan = ISpan(begin, end - 1)
  final def toExclusive: ESpan = this
}

object ESpan {
  private[this] case class ESpanImpl(override val begin: Int, override val end: Int) extends ESpan {
    override def toString = s"[$begin, $end)"
  }
  def apply(x: Int, y: Int): ESpan = ESpanImpl(math.min(x, y), math.max(x, y))
  def unapply(cs: ESpan): Option[(Int, Int)] = ESpanImpl.unapply(cs.asInstanceOf[ESpanImpl])

  // no Lenses because they would not actually be lawful due to the ordering constraint on begin/end

  import io.circe.Encoder
  import io.circe.Decoder
  import io.circe.syntax._

  implicit val espanEncoder: Encoder[ESpan] = Encoder.instance[ESpan] { span =>
    List(span.begin, span.end).asJson
  }

  implicit val espanDecoder: Decoder[ESpan] = Decoder.instance { c =>
    c.as[List[Int]].map(l => ESpan(l(0), l(1)))
  }

  implicit val espanShow: Show[ESpan] = Show.show[ESpan](_.toString)

  implicit val espanOrder: Order[ESpan] = Order.whenEqual(
    Order.by[ESpan, Int](_.begin),
    Order.by[ESpan, Int](_.end),
    )

  implicit val espanSemigroup: Semigroup[ESpan] = new Semigroup[ESpan] {
    def combine(x: ESpan, y: ESpan) = ESpan(math.min(x.begin, y.begin), math.max(x.end, y.end))
  }
}
