package jjm.ling.en

import jjm.LowerCaseString
import jjm.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class InflectedForms(
  stem: LowerCaseString,
  presentSingular3rd: LowerCaseString,
  presentParticiple: LowerCaseString,
  past: LowerCaseString,
  pastParticiple: LowerCaseString
) {

  import VerbForm._
  def apply(form: VerbForm): LowerCaseString = form match {
    case Stem               => stem
    case PresentSingular3rd => presentSingular3rd
    case PresentParticiple  => presentParticiple
    case Past               => past
    case PastParticiple     => pastParticiple
  }

  def getForm(verb: LowerCaseString): Option[VerbForm] =
    if (verb == stem) Some(Stem)
    else if (verb == presentSingular3rd) Some(PresentSingular3rd)
    else if (verb == presentParticiple) Some(PresentParticiple)
    else if (verb == past) Some(Past)
    else if (verb == pastParticiple) Some(PastParticiple)
    else None

  def allForms: List[LowerCaseString] =
    List(stem, presentSingular3rd, presentParticiple, past, pastParticiple)

  override def toString = s"InflectedForms($stem, $presentSingular3rd, $presentParticiple, $past, $pastParticiple)"
}

object InflectedForms {

  def fromStrings(
    stem: String,
    presentSingular3rd: String,
    presentParticiple: String,
    past: String,
    pastParticiple: String
  ) =
    InflectedForms(
      stem.lowerCase,
      presentSingular3rd.lowerCase,
      presentParticiple.lowerCase,
      past.lowerCase,
      pastParticiple.lowerCase
    )

  val generic = InflectedForms.fromStrings(
    "stem", "presentSingular3rd", "presentParticiple", "past", "pastParticiple"
  )

  val doForms = InflectedForms.fromStrings("do", "does", "doing", "did", "done")

  val beSingularForms =
    InflectedForms.fromStrings("be", "is", "being", "was", "been")

  val haveForms = InflectedForms.fromStrings(
    "have",
    "has",
    "having",
    "had",
    "had"
  )
}
