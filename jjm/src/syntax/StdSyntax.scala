package jjm.syntax

trait StdSyntax {

  implicit class RichList[A](val a: List[A]) { // TODO AnyVal
    def remove(i: Int) = a.take(i) ++ a.drop(i + 1)
  }

  implicit class RichOption[A](val a: Option[A]) { // TODO AnyVal
    // more readable alternatives to forall/exists
    def emptyOr(predicate: A => Boolean): Boolean = a.forall(predicate)
    def nonEmptyAnd(predicate: A => Boolean): Boolean = a.exists(predicate)

    def ifEmpty[B](b: => B): Option[B] = a match {
      case Some(_) => None
      case None    => Some(b)
    }
  }

  final implicit class RichAny[A](val a: A) { // TODO AnyVal
    def <|[B](f: A => B): B = f(a)

    def const: Any => A = _ => a

    def onlyIf(p: (A => Boolean)): Option[A] = Some(a).filter(p)
    def ifNot(p: (A => Boolean)): Option[A] = Some(a).filterNot(p)
  }

  // implicit class RichTry[A](val t: Try[A]) extends AnyVal {
  //   // TODO probably nope, maybe after adding in log4cats or something
  //   def toOptionLogging(logger: Logger): Option[A] = t match {
  //     case Success(a) =>
  //       Some(a)
  //     case Failure(e) =>
  //       val sw = new StringWriter()
  //       val pw = new PrintWriter(sw, true)
  //       e.printStackTrace(pw)
  //       logger.error(e.getLocalizedMessage + "\n" + sw.getBuffer.toString)
  //       None
  //   }
  // }

  implicit class RichListCompanion[A](val companion: List.type) { // TODO AnyVal

    def unfold[A, B](a: A, f: A => Option[(B, A)]): List[B] = f(a) match {
      case None                   => Nil
      case Some((head, tailToGo)) => head :: unfold(tailToGo, f)
    }

    def unfold[A, B](a: A, f: PartialFunction[A, (B, A)]): List[B] =
      unfold(a, f.lift)
  }
}
