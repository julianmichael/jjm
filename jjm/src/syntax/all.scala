package jjm.syntax

trait AllSyntax
    extends CatsSyntax
    with DotSyntax
    with LowerCaseStringSyntax
    with StdSyntax
    with TokenSyntax
    with jjm.Finite.ToFiniteOps

object all extends AllSyntax
