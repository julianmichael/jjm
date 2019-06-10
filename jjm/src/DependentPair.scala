package jjm

sealed trait DependentPair[F[_], G[_]] extends Product with Serializable {
  type A
  val fst: F[A]
  val snd: G[A]
}

object DependentPair {
  private[this] case class DependentPairImpl[F[_], G[_], A0](
    override val fst: F[A0],
    override val snd: G[A0]
  ) extends DependentPair[F, G] {
    type A = A0
    override def toString = s"DependentPair($fst, $snd)"
  }

  def apply[F[_], G[_], A0](fst: F[A0], snd: G[A0]): DependentPair[F, G] { type A = A0 } =
    DependentPairImpl[F, G, A0](fst, snd)
  def unapply[F[_], G[_], A](dp: DependentPair[F, G]): Option[(F[A], G[A])] =
    DependentPairImpl.unapply(dp.asInstanceOf[DependentPairImpl[F, G, A]])

  import io.circe.Encoder
  import io.circe.Decoder
  import io.circe.syntax._

  implicit def dependentPairEncoder[F[_], G[_]](
    implicit fstEncoder: Encoder[F[_]],
    sndDependentEncoder: DependentEncoder[F, G]
  ): Encoder[DependentPair[F, G]] = Encoder.instance[DependentPair[F, G]] { pair =>
    implicit val sndEncoder = sndDependentEncoder(pair.fst)
    List(fstEncoder(pair.fst).asJson, sndEncoder(pair.snd).asJson).asJson
  }

  // TODO decoder

  // // TODO Really would like to fix up the Decoder.
  // // Not sure how to get the types to work out without this auxiliary extra Foo
  // // to hold the type skolem

  // private case class Foo[F[_], A](fa: F[A]) {
  //   type Arg = A
  // }

  // implicit def dependentMapDecoder[F[_], G[_]](
  //   implicit keyDecoder: KeyDecoder[F[_]],
  //   dependentDecoder: DependentDecoder[F, G]
  // ): Decoder[DependentMap[F, G]] = new Decoder[DependentMap[F, G]] {
  //   final def apply(c: HCursor): Decoder.Result[DependentMap[F, G]] = {
  //     // TODO aah replace the get
  //     c.keys.get.toList.foldM[Decoder.Result, DependentMap[F, G]](DependentMap.empty[F, G]) { (m, keyStr) =>
  //       import scala.language.existentials
  //       val key = keyDecoder(keyStr).get // TODO aah replace the get
  //       val value = dependentDecoder(key).tryDecode(c.downField(keyStr))
  //       val foo = Foo(key)
  //       type Arg = foo.Arg
  //       value.map(v => m.put[Arg](key.asInstanceOf[F[Arg]], v.asInstanceOf[G[Arg]]))
  //     }
  //   }
  // }
}
