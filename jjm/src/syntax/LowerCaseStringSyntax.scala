package jjm.syntax

import jjm.LowerCaseString

import scala.language.implicitConversions

trait LowerCaseStringSyntax0 {
  // low-pri conversion to make it easy to use in place of strings
  implicit def lowerCaseStringToString(s: LowerCaseString): String = LowerCaseString.toString(s)
}
trait LowerCaseStringSyntax extends LowerCaseStringSyntax0 {
  implicit def toStringOps(s: String): LowerCaseString.StringOps = {
    LowerCaseString.getStringOps(s)
  }
  implicit def toLowerCaseStringOps(s: LowerCaseString): LowerCaseString.LowerCaseStringOps = {
    LowerCaseString.getLowerCaseStringOps(s)
  }
}
