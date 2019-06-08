package jjm.ling

import cats.Id
import cats.Foldable
import cats.Monad
import cats.Monoid
import cats.implicits._
// import cats.data._
// import scala.language.higherKinds

import HasTokens.ops._
import HasAlignedTokens.ops._

/** Provides method(s) for rendering text from a list of tokens. */
object Text {
  private val noSpaceBefore = Set(
    ".",
    ",",
    "!",
    "?",
    ";",
    ":",
    "''",
    "n't",
    "'s",
    "'re",
    "'ve",
    "'ll",
    "na",
    "'m",
    "'d",
    // "''", // TODO hmm maybe, maybe not
    "%",
    "-",
    "+",
    "-RRB-",
    "-RCB-",
    "-RSB-",
    ")",
    "]",
    "}",
    "/.",
    "/?",
    "°"
  )

  private val noSpaceAfter = Set(
    "``",
    "$",
    "£",
    "€",
    "#",
    "-",
    "-LRB-",
    "-LCB-",
    "-LSB-",
    "(",
    "[",
    "{"
  )

  // what does this mean... lol... it appears in the bc (broadcast conversation) domain of OntoNotes
  private val hidden = Set(
    "%pw"
  )

  // val sentenceEndings = Set(
  //   "'", "\"", ".", ",", "!", "?", ";", ":"
  // )

  // TODO ;; -> ; ?
  /** Normalize a Penn Treebank token to its string representations. */
  def normalizeToken(token: String) = token match {
    case "`"     => "'"
    case "``"    => "\""
    case "''"    => "\""
    case "-LRB-" => "("
    case "-RRB-" => ")"
    case "-LCB-" => "{"
    case "-RCB-" => "}"
    case "-LSB-" => "["
    case "-RSB-" => "]"
    case "/."    => "."
    case "/?"    => "?"
    case "/-"    => "-"
    case "--"    => "-"
    case w       => w.replaceAll("\\\\/", "/")
  }

  /**
    * Returns a best-effort properly spaced representation of a sequence of tokens.
    * (Bear in mind you need to normalize PTB tokens yourself inside the renderWord parameter.)
    * Allows you to specify how to render spaces and words so you can use this to create interactive DOM elements in JS.
    * And it's monadic.
    */
  def renderM[Word, F[_]: Foldable, M[_]: Monad, Result: Monoid](
    words: F[Word],
    getToken: Word => String,
    spaceFromNextWord: Word => M[Result],
    renderWord: Word => M[Result]
  ): M[Result] = {
    val hasSingleQuoteOnly = words.toList
      .map(getToken)
      .filter(t => t == "'" || t == "`")
      .size == 1
    words
      .foldM[M, (Result, Boolean, Boolean, Boolean)]((Monoid[Result].empty, true, false, false)) {
        case ((acc, skipSpace, insideSingleQuotes, insideDoubleQuotes), word) =>
          val token = getToken(word)
          if (hidden.contains(token)) { // skip token / do nothing
            Monad[M].pure((acc, skipSpace, insideSingleQuotes, insideDoubleQuotes))
          } else {
            val (skipPrevSpace, skipNextSpace, nowInsideSingleQuotes, nowInsideDoubleQuotes) =
              if (hasSingleQuoteOnly && token == "'")
                (
                  true,
                  false,
                  false,
                  insideDoubleQuotes
                )
              else
                (
                  // skip prev space
                  skipSpace ||
                  (insideSingleQuotes && token.equals("'")) ||
                  (insideDoubleQuotes && (token.equals("''") || token.equals("\""))),
                  // skip next space
                  noSpaceAfter.contains(normalizeToken(token)) ||
                  (!insideSingleQuotes && (token.equals("`") || token.equals("'"))) ||
                  (!insideDoubleQuotes && (token.equals("``") || token.equals("\""))),
                  // now inside single
                  (token.equals("'") || token.equals("`")) ^ insideSingleQuotes,
                  // now inside double
                  (token.equals("''") || token.equals("``") || token.equals("\"")) ^ insideDoubleQuotes
                )

            if (skipPrevSpace || noSpaceBefore.contains(normalizeToken(token))) {
              for {
                w <- renderWord(word)
              } yield {
                (acc |+| w, skipNextSpace, nowInsideSingleQuotes, nowInsideDoubleQuotes)
              }
            } else {
              for {
                space <- spaceFromNextWord(word)
                w     <- renderWord(word)
              } yield {
                (acc |+| space |+| w, skipNextSpace, nowInsideSingleQuotes, nowInsideDoubleQuotes)
              }
            }
          }
      }
      .map(_._1)
  }

  /** Monadic convenience method for rendering anything that HasTokens */
  def renderM[A: HasTokens, M[_]: Monad, Result: Monoid](
    input: A,
    spaceFromNextToken: String => M[Result],
    renderToken: String => M[Result]
  ): M[Result] =
    renderM[String, Vector, M, Result](input.tokens, identity, spaceFromNextToken, renderToken)

  /** Non-monadic convenience method for renderM with tokens */
  def render[Word, F[_]: Foldable, M: Monoid](
    words: F[Word],
    getToken: Word => String,
    spaceFromNextWord: Word => M,
    renderWord: Word => M
  ): M =
    renderM[Word, F, Id, M](words, getToken, spaceFromNextWord, renderWord)

  /** Non-monadic convenience method for rendering anything that HasTokens */
  def render[A: HasTokens, Result: Monoid](
    input: A,
    spaceFromNextToken: String => Result,
    renderToken: String => Result
  ): Result = {
    render[String, Vector, Result](input.tokens, identity, spaceFromNextToken, renderToken)
  }

  /** Convenience method for rendering a sequence of PTB tokens directly to a string. */
  def render[F[_]: Foldable](tokens: F[String]): String =
    render[String, F, String](tokens, identity, (_: String) => " ", normalizeToken(_))

  /** Convenience method for rendering something that HasTokens directly to a string */
  def render[A: HasTokens](input: A): String = render(input.tokens)

  // TODO make the HasTokens ones respect spaces so the result is ACTUALLY always a substring

  /** Render a substring of a list of tokens */
  def renderSpan[F[_]: Foldable](reference: F[String], span: Set[Int]) =
    render(reference.toList.zipWithIndex.filter(p => span.contains(p._2)).map(_._1))

  /** Render a substring of a something that HasTokens */
  def renderSpan[A: HasTokens](reference: A, span: Set[Int]) =
    render(
      reference.tokens.toList.zipWithIndex
        .filter(p => span.contains(p._2))
        .map(_._1)
    )

  // TODO implement span re-recovery from a string

  // aligned text rendering

  /** Returns a correct rendering of aligned tokens that preserves their whitespace. */
  def renderAlignedM[Word, F[_]: Foldable, M[_]: Monad, Result: Monoid](
    words: F[Word],
    getAlignedToken: Word => AlignedToken,
    spaceFromSurroundingWords: (Option[Word], String, Option[Word]) => M[Result],
    renderWord: Word => M[Result]
  ): M[Result] = {
    val resultPairM =
      words.foldM[M, (Result, Option[Word])](Monoid[Result].empty, None) {
        case ((acc, prevWordOpt), word) =>
          val alignedToken = getAlignedToken(word)
          for {
            space <- spaceFromSurroundingWords(
              prevWordOpt,
              alignedToken.whitespaceBefore,
              Some(word)
            )
            w <- renderWord(word)
          } yield (acc |+| space |+| w, Some(word))
      }
    for {
      resultPair <- resultPairM
      result <- resultPair._2 match {
        case None => Monad[M].pure(Monoid[Result].empty)
        case Some(word) =>
          val alignedToken = getAlignedToken(word)
          for {
            space <- spaceFromSurroundingWords(Some(word), alignedToken.whitespaceAfter, None)
          } yield resultPair._1 |+| space
      }
    } yield result

  }

  /** Monadic convenience method for rendering anything that HasAlignedTokens */
  def renderAlignedM[A: HasAlignedTokens, M[_]: Monad, Result: Monoid](
    input: A,
    spaceFromSurroundingWords: (Option[AlignedToken], String, Option[AlignedToken]) => M[Result],
    renderOriginalToken: String => M[Result]
  ): M[Result] =
    renderAlignedM[AlignedToken, Vector, M, Result](
      input.alignedTokens,
      identity,
      spaceFromSurroundingWords,
      (alignedToken: AlignedToken) => renderOriginalToken(alignedToken.originalText)
    )

  /** Non-monadic convenience method for renderM with aligned tokens */
  def renderAligned[Word, F[_]: Foldable, Result: Monoid](
    words: F[Word],
    getAlignedToken: Word => AlignedToken,
    spaceFromSurroundingWords: (Option[Word], String, Option[Word]) => Result,
    renderWord: Word => Result
  ): Result =
    renderAlignedM[Word, F, Id, Result](
      words,
      getAlignedToken,
      spaceFromSurroundingWords,
      renderWord
    )

  /** Non-monadic convenience method for rendering anything that HasAlignedTokens */
  def renderAligned[A: HasAlignedTokens, Result: Monoid](
    input: A,
    spaceFromSurroundingWords: (Option[AlignedToken], String, Option[AlignedToken]) => Result,
    renderOriginalToken: String => Result
  ): Result = {
    renderAligned[AlignedToken, Vector, Result](
      input.alignedTokens,
      identity,
      spaceFromSurroundingWords,
      (alignedToken: AlignedToken) => renderOriginalToken(alignedToken.originalText)
    )
  }

  // render aligned tokens that already know about their whitespace
  def renderAligned[F[_]: Foldable](alignedTokens: F[AlignedToken]): String =
    renderAligned[AlignedToken, F, String](
      alignedTokens,
      identity,
      (_: Option[AlignedToken], space: String, _: Option[AlignedToken]) => space,
      (alignedToken: AlignedToken) => alignedToken.originalText
    )

  def renderAligned[A: HasAlignedTokens](input: A): String =
    renderAligned[A, String](
      input,
      (_: Option[AlignedToken], space: String, _: Option[AlignedToken]) => space,
      identity
    )

  def renderAlignedSpan[F[_]: Foldable](reference: F[AlignedToken], span: Set[Int]) = {
    reference.toList.zipWithIndex
      .filter(p => span.contains(p._2))
      .map(_._1) match {
      case Nil =>
        ""
      case firstToken :: restOfTokens =>
        firstToken.originalText + restOfTokens
          .map(t => t.whitespaceBefore + t.originalText)
          .mkString
    }
  }

  def renderAlignedSpan[A: HasAlignedTokens](reference: A, span: Set[Int]) = {
    reference.alignedTokens.toList.zipWithIndex
      .filter(p => span.contains(p._2))
      .map(_._1) match {
      case Nil =>
        ""
      case firstToken :: restOfTokens =>
        firstToken.originalText + restOfTokens
          .map(t => t.whitespaceBefore + t.originalText)
          .mkString
    }
  }
}
