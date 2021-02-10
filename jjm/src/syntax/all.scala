package jjm.syntax

trait AllSyntax
    extends CatsSyntax
    with DotSyntax
    with LowerCaseStringSyntax
    with StdSyntax
    with TokenSyntax
    with jjm.Finite.ToFiniteOps
    with jjm.metrics.HasMetrics.ToHasMetricsOps

object all extends AllSyntax
