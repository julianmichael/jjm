package jjm.ling

case class TokenText(
  token: String,
  whitespaceBefore: String,
  whitespaceAfter: String) {
  def text: String = whitespaceBefore + token + whitespaceAfter
}
object TokenText {
  def fromText(text: String) = {
    val res = TokenText(
      text.trim,
      text.takeWhile(_.isWhitespace),
      text.reverse.takeWhile(_.isWhitespace).reverse
    )
    assert(res.text == text, s"TokenText text [${res.text}] must equal source [$text] it was derived from")
    res
  }

  import io.circe.{Encoder, Decoder}

  implicit val tokenTextEncoder: Encoder[TokenText] = Encoder.encodeString.contramap[TokenText](_.text)
  implicit val tokenTextDecoder: Decoder[TokenText] = Decoder.decodeString.map(fromText)
}
