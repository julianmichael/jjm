package jjm.ling

case class AlignedToken(
  token: String,
  originalText: String,
  whitespaceBefore: String,
  whitespaceAfter: String
)
