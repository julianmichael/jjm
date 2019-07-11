package jjm.syntax

trait AllSyntax
    extends CatsSyntax
    with DotSyntax
    with LowerCaseStringSyntax
    with StdSyntax
    with TokenSyntax

object all extends AllSyntax
