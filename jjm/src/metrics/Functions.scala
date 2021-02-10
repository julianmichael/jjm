package jjm.metrics

object Functions {

  def harmonicMean(x: Double, y: Double) = {
    val denom = x + y
    if(denom == 0.0) 0.0 else {
      2 * x * y / (x + y)
    }
  }

  def weightedHarmonicMean(beta: Double, x: Double, y: Double) = {
    val betaSq = beta * beta
    val denom = (betaSq * x) + y
    if(denom == 0.0) 0.0 else {
      (1 + betaSq) * x * y / denom
    }
  }

  // def mapSqEuclideanDistance[A](x: Map[A, Double], y: Map[A, Double]) = {
  //   val keys = x.keySet ++ y.keySet
  //   keys.unorderedFoldMap(k => scala.math.pow(x.getOrElse(k, 0.0) - y.getOrElse(k, 0.0), 2))
  // }

  // def mapEntropy[A](x: Map[A, Double]): Double = {
  //   val total = x.unorderedFold
  //   - x.unorderedFoldMap(v => scala.math.log(v / total) * v / total)
  // }

  // def mapJensenShannonDivergence[A](x: Map[A, Double], y: Map[A, Double]) = {
  //   val xTot = x.unorderedFold; val yTot = y.unorderedFold
  //   val xx = x.mapVals(_ / xTot); val yy = y.mapVals(_ / yTot)
  //   val mixture = (xx |+| yy).mapVals(_ / 2)
  //   mapEntropy(mixture) - ((mapEntropy(xx) + mapEntropy(yy) / 2))
  // }

}
