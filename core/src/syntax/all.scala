package jjm.syntax

trait AllSyntax
    extends CatsSyntax
    with LowerCaseStringSyntax
    with StdSyntax

object all extends AllSyntax
