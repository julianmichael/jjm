package jjm

import io.circe.{Encoder, Decoder}

/** Bidirectional index. */
class Vocab[A] private (
  val indexToItem: Vector[A],
  val itemToIndex: Map[A, Int]
) {
  def items = indexToItem
  def indices = indexToItem.indices.toVector
  def getItem(index: Int) = indexToItem(index)
  def getIndex(item: A) = itemToIndex(item)
  def size = indexToItem.size
}
object Vocab {
  def make[A](items: Set[A]): Vocab[A] = {
    val itemsVec = items.toVector
    new Vocab(itemsVec, itemsVec.zipWithIndex.toMap)
  }
  def fromSet[A](items: Set[A]): Vocab[A] = make(items)

  def fromVectorUnsafe[A](items: Vector[A]): Vocab[A] = {
    new Vocab(items, items.zipWithIndex.toMap)
  }

  implicit def vocabEncoder[A: Encoder] =
    implicitly[Encoder[Vector[A]]].contramap[Vocab[A]](_.items)
  implicit def vocabDecoder[A: Decoder] =
    implicitly[Decoder[Vector[A]]].map(fromVectorUnsafe[A])
}
