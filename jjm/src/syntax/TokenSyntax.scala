package jjm.syntax

import jjm.ling._

trait TokenSyntax
    extends HasToken.ToHasTokenOps
    with HasSourceText.ToHasSourceTextOps
    with HasIndex.ToHasIndexOps
    with HasPos.ToHasPosOps
    with HasLemma.ToHasLemmaOps
