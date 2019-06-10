package jjm.ling.en

sealed trait VerbForm
case object Stem extends VerbForm
case object PresentSingular3rd extends VerbForm
case object PresentParticiple extends VerbForm
case object Past extends VerbForm
case object PastParticiple extends VerbForm

object VerbForm {
  def allForms: List[VerbForm] =
    List(Stem, PresentSingular3rd, PresentParticiple, Past, PastParticiple)
}
