package jjm.corenlp

import jjm.ling.Token
import jjm.ling.TokenText
import jjm.ling.SourceText

import shapeless.HNil

object Tokenizer {

  /** Tokenizes an English string. */
  def tokenize(s: String) = {
    import java.io.StringReader
    import edu.stanford.nlp.process.PTBTokenizer
    import edu.stanford.nlp.process.WordTokenFactory
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new WordTokenFactory(), "").tokenize.asScala.toVector
      .map(t => Token(t.word))
  }

  def tokenizeWithSource(s: String) = {
    import java.io.StringReader
    import edu.stanford.nlp.process.PTBTokenizer
    import edu.stanford.nlp.process.CoreLabelTokenFactory
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new CoreLabelTokenFactory(), "invertible=true")
      .tokenize.asScala.toVector.map(coreLabel =>
        Token.field(coreLabel.word)
          :: SourceText.field(
            TokenText(
              token = coreLabel.originalText,
              whitespaceBefore = coreLabel.before,
              whitespaceAfter = coreLabel.after
            ))
          :: HNil
      )
  }
}
