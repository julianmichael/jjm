package jjm.ling

package object en /*extends PackagePlatformExtensions */{

  // val whWords = Set("who", "what", "when", "where", "why", "how", "which", "whose").map(_.lowerCase)

  def simpleTokenize(s: String): Vector[String] =
    s.split("(\\s+|[.,;!?.'\"])").toVector

}
