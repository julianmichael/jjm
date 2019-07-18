package jjm.ling.en

sealed trait VerbForm {
  import VerbForm._
  override def toString: String = this match {
    case Stem               => "stem"
    case PresentSingular3rd => "presentSingular3rd"
    case PresentParticiple  => "presentParticiple"
    case Past               => "past"
    case PastParticiple     => "pastParticiple"
  }
}

object VerbForm {
  case object Stem extends VerbForm
  case object PresentSingular3rd extends VerbForm
  case object PresentParticiple extends VerbForm
  case object Past extends VerbForm
  case object PastParticiple extends VerbForm

  def allForms: List[VerbForm] =
    List(Stem, PresentSingular3rd, PresentParticiple, Past, PastParticiple)
}
