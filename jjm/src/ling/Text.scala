package jjm.ling

import jjm.implicits._

import cats.Id
import cats.Foldable
import cats.Monad
import cats.Monoid
import cats.Traverse
import cats.implicits._

import shapeless.HList
import shapeless.ops.record.Updater
import shapeless.record._

/** Provides method(s) for rendering / manipulating text. */
object Text {

  def addIndices[F[_]: Traverse, R <: HList : Updater[*, Index]](tokens: F[R]) = {
    tokens.mapWithIndex((r, i) => r.updated(Index, i))
  }

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
  def renderTokensM[A, F[_]: Foldable, M[_]: Monad, Result: Monoid](
    words: F[A],
    getToken: A => String,
    spaceFromNextWord: A => M[Result],
    renderWord: A => M[Result]
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
  def renderTokensM[A: HasToken, F[_]: Foldable, M[_]: Monad, Result: Monoid](
    input: F[A],
    spaceFromNextToken: A => M[Result],
    renderToken: String => M[Result]
  ): M[Result] =
    renderTokensM[A, F, M, Result](
      input,
      (a: A) => a.token,
      spaceFromNextToken,
      (a: A) => renderToken(a.token)
    )

  /** Non-monadic convenience method for renderM with tokens */
  def renderTokens[A, F[_]: Foldable, M: Monoid](
    words: F[A],
    getToken: A => String,
    spaceFromNextWord: A => M,
    renderWord: A => M
  ): M =
    renderTokensM[A, F, Id, M](words, getToken, spaceFromNextWord, renderWord)

  /** Non-monadic convenience method for rendering anything that HasTokens */
  def renderTokens[A: HasToken, F[_]: Foldable, Result: Monoid](
    input: F[A],
    spaceFromNextToken: A => Result,
    renderToken: String => Result
  ): Result = {
    renderTokens[A, F, Result](input, (a: A) => a.token, spaceFromNextToken, (a: A) => renderToken(a.token))
  }

  /** Convenience method for rendering a sequence of PTB tokens directly to a string. */
  def renderTokens[A: HasToken, F[_]: Foldable](tokens: F[A]): String =
    renderTokens[A, F, String](tokens, (_: A) => " ", normalizeToken(_))

  // TODO make the HasTokens ones respect spaces so the result is ACTUALLY always a substring

  /** Render a substring of a list of tokens */
  def renderSpanTokens[A: HasToken : HasIndex, F[_]: Foldable](reference: F[A], span: Span) =
    renderTokens(reference.toList.filter(p => span.contains(p.index)))

  // TODO implement span re-recovery from a string

  /** Returns a correct rendering of aligned tokens that preserves their whitespace. */
  def renderSourceM[A, F[_]: Foldable, M[_]: Monad, Result: Monoid](
    words: F[A],
    getSourceText: A => TokenText,
    spaceFromSurroundingWords: (Option[A], String, Option[A]) => M[Result],
    renderWord: A => M[Result]
  ): M[Result] = {
    val resultPairM =
      words.foldM[M, (Result, Option[A])](Monoid[Result].empty, None) {
        case ((acc, prevWordOpt), word) =>
          val alignedToken = getSourceText(word)
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
          val alignedToken = getSourceText(word)
          for {
            space <- spaceFromSurroundingWords(Some(word), alignedToken.whitespaceAfter, None)
          } yield resultPair._1 |+| space
      }
    } yield result

  }

  /** Monadic convenience method for rendering anything that has source text */
  def renderSourceM[A: HasSourceText, F[_]: Foldable, M[_]: Monad, Result: Monoid](
    input: F[A],
    spaceFromSurroundingWords: (Option[A], String, Option[A]) => M[Result],
    renderOriginalToken: String => M[Result]
  ): M[Result] =
    renderSourceM[A, F, M, Result](
      input,
      (a: A) => a.sourceText,
      spaceFromSurroundingWords,
      (a: A) => renderOriginalToken(a.sourceText.token)
    )

  /** Non-monadic convenience method for renderM with aligned tokens */
  def renderSource[A, F[_]: Foldable, Result: Monoid](
    words: F[A],
    getSourceText: A => TokenText,
    spaceFromSurroundingWords: (Option[A], String, Option[A]) => Result,
    renderWord: A => Result
  ): Result =
    renderSourceM[A, F, Id, Result](
      words,
      getSourceText,
      spaceFromSurroundingWords,
      renderWord
    )

  /** Non-monadic convenience method for rendering anything that has source text */
  def renderSource[A: HasSourceText, F[_]: Foldable, Result: Monoid](
    input: F[A],
    spaceFromSurroundingWords: (Option[A], String, Option[A]) => Result,
    renderOriginalToken: String => Result
  ): Result = {
    renderSource[A, F, Result](
      input,
      (a: A) => a.sourceText,
      spaceFromSurroundingWords,
      (a: A) => renderOriginalToken(a.sourceText.token)
    )
  }

  def renderSource[A: HasSourceText, F[_]: Foldable](input: F[A]): String =
    renderSource[A, F, String](
      input,
      (_: Option[A], space: String, _: Option[A]) => space,
      identity[String](_)
    )

  def renderSpanSource[A: HasSourceText : HasIndex, F[_]: Foldable](reference: F[A], span: Span) = {
    reference.toList.filter(p => span.contains(p.index)).map(_.sourceText) match {
      case Nil =>
        ""
      case first :: rest =>
        first.token + rest
          .map(t => t.whitespaceBefore + t.token)
          .mkString
    }
  }

  trait TextRenderer[A, F[_]] { def apply(input: F[A]): String }
  sealed trait TextRendererLowPriority0 {
    implicit def stringRenderer[F[_]: Foldable] = new TextRenderer[String, F] {
      def apply(input: F[String]) = renderTokens(input.toList.map(Token(_)))
    }
    implicit def tokenTextRenderer[F[_]: Foldable] = new TextRenderer[TokenText, F] {
      def apply(input: F[TokenText]) = renderSource(input.toList.map(SourceText(_)))
    }
  }
  sealed trait TextRendererLowPriority extends TextRendererLowPriority0 {
    implicit def tokenRenderer[A: HasToken, F[_]: Foldable] = new TextRenderer[A, F] {
      def apply(input: F[A]) = renderTokens(input)
    }
  }
  object TextRenderer extends TextRendererLowPriority {
    implicit def sourceRenderer[A: HasSourceText, F[_]: Foldable] = new TextRenderer[A, F] {
      def apply(input: F[A]) = renderSource(input)
    }
  }

  trait SpanRenderer[A, F[_]] { def apply(input: F[A], span: Span): String }
  sealed trait SpanRendererLowPriority0 {
    implicit def stringRenderer[F[_]: Foldable] = new SpanRenderer[String, F] {
      def apply(input: F[String], span: Span) = renderSpanTokens(addIndices(input.toList.map(Token(_))), span)
    }
    implicit def tokenTextRenderer[F[_]: Foldable] = new SpanRenderer[TokenText, F] {
      def apply(input: F[TokenText], span: Span) = renderSpanSource(addIndices(input.toList.map(SourceText(_))), span)
    }
    implicit def indexingTokenRenderer[A: HasToken, F[_]: Foldable] = new SpanRenderer[A, F] {
      def apply(input: F[A], span: Span) = renderSpanTokens(addIndices(input.toList.map(t => Token(t.token))), span)
    }
  }
  sealed trait SpanRendererLowPriority extends SpanRendererLowPriority0 {
    implicit def tokenRenderer[A: HasToken : HasIndex, F[_]: Foldable] = new SpanRenderer[A, F] {
      def apply(input: F[A], span: Span) = renderSpanTokens(input, span)
    }
    implicit def indexingSourceRenderer[A: HasSourceText, F[_]: Foldable] = new SpanRenderer[A, F] {
      def apply(input: F[A], span: Span) = renderSpanSource(addIndices(input.toList.map(t => SourceText(t.sourceText))), span)
    }
  }
  object SpanRenderer extends SpanRendererLowPriority {
    implicit def sourceRenderer[A: HasSourceText : HasIndex, F[_]: Foldable] = new SpanRenderer[A, F] {
      def apply(input: F[A], span: Span) = renderSpanSource(input, span)
    }
  }

  // choose at compile time to render source if possible, otherwise fall back to tokens

  def render[A, F[_]](input: F[A])(implicit render: TextRenderer[A, F]): String = render(input)
  def renderSpan[A, F[_]](input: F[A], span: Span)(implicit render: SpanRenderer[A, F]): String = render(input, span)
}
