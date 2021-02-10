package jjm.ling.en

import cats.Foldable
import cats.implicits._

import jjm.LowerCaseString
import jjm.implicits._

import java.nio.file.Files
import java.nio.file.Path

/** Class for easy access to verb inflections.
  *
  * Backed by Java code (VerbInflectionDictionary)
  * that loads a list of verb forms from a local text file that was scraped
  * from Wiktionary. TODO: make it cross-platform.
  *
  * @param inflDict the backing inflection dictionary
  */
class Inflections(private[this] val inflDict: VerbInflectionDictionary) {

  import Inflections._

  /** Returns the inflected forms for the given verb if it is in the dictionary.
    * If it's hyphenated, operates on the suffix after the first hyphen
    * and replaces the prefix at the beginning.
    *
    * Also note that this does not handle be-verbs.
    * TODO: handle these in the future?
    *
    * @param word a verb (stem not necessary)
    * @return the inflections of the given verb
    */
  def getInflectedForms(verb: LowerCaseString): Option[InflectedForms] = {
    val (verbPrefixOpt, verbSuffix) = verb.indexOf("-") match {
      case -1 => (None, verb)
      case i  => (Some(verb.substring(0, i)), verb.substring(i + 1))
    }
    Option(inflDict.getBestInflections(verbSuffix)).map(_.toVector).map { l =>
      val forms = verbPrefixOpt.fold(l.map(_.lowerCase))(
        prefix => l.map(suffix => s"$prefix-$suffix".lowerCase)
      )
      InflectedForms(
        stem = forms(0),
        presentSingular3rd = forms(1),
        presentParticiple = forms(2),
        past = forms(3),
        pastParticiple = forms(4)
      )
    }
  }

  /** Returns a set of all known inflected forms of a verb.
    *
    * This handles auxiliary & other verbs. It is designed to also include irregular forms,
    * for example dream(past) -> { dreamed, dreamt }, if we add them manually to extraForms
    * (see Inflections companion object).
    * Also includes contractions.
    *
    * @param word a verb (stem not necessary)
    * @return all possible forms of the verb
    */
  def getAllForms(word: LowerCaseString): Set[LowerCaseString] = {
    val extras: Set[LowerCaseString] = extraForms
      .get(getUninflected(word).getOrElse(word))
      .getOrElse(Set.empty[LowerCaseString])
    List(doVerbs, beVerbs, willVerbs, haveVerbs, wouldVerbs, negationWords)
      .filter(_.contains(word))
      .headOption
      .orElse(getInflectedForms(word).map(_.allForms.toSet))
      .getOrElse(Set(word)) ++ extras
  }

  /** Whether a word is present in the dictionary, auxiliary verbs not included. */
  def hasInflectedForms(word: LowerCaseString) =
    !getInflectedForms(word).isEmpty

  /** The stem of a verb, including "be". */
  def getUninflected(word: LowerCaseString): Option[LowerCaseString] = {
    if (isCopulaVerb(word)) {
      return Some("be".lowerCase);
    } else {
      Option(inflDict.getBestInflections(word))
        .map(infl => infl(0).lowerCase)
    }
  }

  /** Whether a word is a known verb stem. */
  def isUninflected(word: LowerCaseString): Boolean =
    getUninflected(word).exists(_ == word)

  /** Whether a word is a known modal verb. */
  def isModal(word: LowerCaseString) = modalVerbs.contains(word)

  /** Whether a word is a known copula. */
  def isCopulaVerb(word: LowerCaseString) = beVerbs.contains(word)

  /** Normalizes a modal (i.e., undoes its contraction form), otherwise identity. */
  def getNormalizedModal(verb: LowerCaseString) =
    if (verb == "ca".lowerCase) "can"
    else if (verb == "wo".lowerCase) "will"
    else verb

  // TODO: add tense and stuff. not necessary right now.
  // See VerbHelper from the HITL project in EasySRL for more.
}

object Inflections {
  final val doVerbs = Set("do", "does", "doing", "did", "done").map(_.lowerCase)
  final val beVerbs =
    Set("be", "being", "been", "am", "'m", "is", "'s", "ai", "are", "'re", "was", "were").map(
      _.lowerCase
    )
  val willVerbs = Set("will", "'ll", "wo").map(_.lowerCase)

  val haveVerbs =
    Set("have", "having", "'ve", "has", "had", "'d").map(_.lowerCase)
  val wouldVerbs = Set("would", "'d").map(_.lowerCase)

  val modalVerbs = Set("can", "ca", "could", "may", "might", "must", "shall", "should", "ought")
    .map(_.lowerCase) ++ wouldVerbs

  val auxiliaryVerbs = doVerbs ++ beVerbs ++ willVerbs ++ haveVerbs ++ modalVerbs
  val negationWords = Set("no", "not", "n't").map(_.lowerCase)

  /** Maps an uninflected verb to extra forms of it that aren't in wiktionary. */
  val extraForms = Map[LowerCaseString, Set[LowerCaseString]](
    "dream".lowerCase -> Set("dreamt").map(_.lowerCase),
    "leap".lowerCase  -> Set("leapt").map(_.lowerCase)
  )

  // TODO factor out logic into pure methods here and then put the IO fn in the jjm-io package
  def loadFromFileForCorpus[F[_]: Foldable](
    wiktionaryPath: Path, corpus: F[String], downloadIfNecessary: Boolean = true
  ): Inflections = ???
    // for {
    //   _ <- IO(Files.exists(wiktionaryPath)).ifM(IO.unit)(
    //     if(downloadIfNecessary) IO.unit // TODO: download from Dropbox
    //     else IO.fail(new FileNotFoundException(wiktionaryPath)) // TODO fix this line
    //   )
    //   wiktionaryInflectionsPath = wiktionaryPath.resolve("en_verb_inflections.txt")
    //   wordDict <- IO {
    //     val dict = new CountDictionary()
    //     corpus.foreach(dict.addString)
    //     dict
    //   }
    //   inflDict <- IO {
    //     new VerbInflectionDictionary(wordDict)
    //       .loadDictionaryFromFile(wiktionaryInflectionsPath.toString)
    //   }
    // } yield new Inflections(inflDict)
}
