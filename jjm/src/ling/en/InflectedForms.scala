package jjm.ling.en

import jjm.LowerCaseString
import jjm.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class InflectedForms(
  stem: LowerCaseString,
  present: LowerCaseString,
  presentParticiple: LowerCaseString,
  past: LowerCaseString,
  pastParticiple: LowerCaseString
) extends (VerbForm => LowerCaseString) {

  def apply(form: VerbForm): LowerCaseString = form match {
    case Stem               => stem
    case PresentSingular3rd => present
    case PresentParticiple  => presentParticiple
    case Past               => past
    case PastParticiple     => pastParticiple
  }

  def getForm(verb: LowerCaseString): Option[VerbForm] =
    if (verb == stem) Some(Stem)
    else if (verb == present) Some(PresentSingular3rd)
    else if (verb == presentParticiple) Some(PresentParticiple)
    else if (verb == past) Some(Past)
    else if (verb == pastParticiple) Some(PastParticiple)
    else None

  def allForms: List[LowerCaseString] =
    List(stem, present, presentParticiple, past, pastParticiple)

  override def toString = s"InflectedForms($stem, $present, $presentParticiple, $past, $pastParticiple)"
}

object InflectedForms {

  def fromStrings(
    stem: String,
    present: String,
    presentParticiple: String,
    past: String,
    pastParticiple: String
  ) =
    InflectedForms(
      stem.lowerCase,
      present.lowerCase,
      presentParticiple.lowerCase,
      past.lowerCase,
      pastParticiple.lowerCase
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
