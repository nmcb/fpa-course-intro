package fpa

object P:
  case class P[A](parse: String => Option[(A, String)]):
    def run(s: String): A =
      parse(s) match
        case Some(a, "") => a
        case Some(_, rest) => sys.error(s"unconsumed at ${rest.take(10)}")
        case None => sys.error(s"failed to parse")

    def map[B](f: A => B): P[B] =
      P(s => parse(s) match
        case Some(a, r) => Some(f(a), r)
        case None => None
      )

    def flatMap[B](f: A => P[B]): P[B] =
      P(s => parse(s) match
        case Some(a, r) => f(a).parse(r)
        case None => None
      )

    private def loop(s: String, acc: List[A] = List.empty): (List[A], String) =
      parse(s) match {
        case None => (acc.reverse, s)
        case Some((a, ss)) => loop(ss, a :: acc)
      }

    def opt: P[Option[A]] =
      P(s => parse(s).map((a, ss) => (Some(a), ss)).orElse(Some(None, s)))

    def zeroOrMore: P[List[A]] =
      P(s => Some(loop(s)))

    def oneOrMore: P[List[A]] =
      P(s => parse(s).flatMap((a, ss) => Some(loop(ss, List(a)))))

    def |[A1 >: A](that: => P[A1]): P[A1] =
      P(s => parse(s) match {
        case None => that.parse(s)
        case res@Some(_) => res
      })

    def &[B](that: => P[B]): P[(A, B)] =
      for {a <- this; b <- that} yield (a, b)

    def ~[B](that: P[B]): P[B] =
      for {_ <- this; b <- that} yield b

  def unit[A](a: A): P[A] =
    P(s => Some(a, s))

  def fail[A]: P[A] =
    P(_ => None)

  def take: P[Char] =
    P(s => if s.nonEmpty then Some(s.head, s.tail) else None)

  def satisfy(p: Char => Boolean): P[Char] =
    take.flatMap(c => if p(c) then unit(c) else fail)

  def char(c: Char): P[Char] =
    satisfy(_ == c)

  def digit: P[Char] =
    satisfy(_.isDigit)

  def digits: P[Int] =
    satisfy(_.isDigit).oneOrMore.map(_.mkString("").toInt)

  def separated[A](sep: P[?], pa: P[A]): P[List[A]] =
    for {h <- pa.opt; t <- (sep ~ pa).zeroOrMore} yield
      h.map(_ :: t).getOrElse(List.empty)

  def separated[A](sep: Char, pa: P[A]): P[List[A]] =
    separated(char(sep), pa)

  def seq[A](lhs: Char, sep: Char, rhs: Char, pa: P[A]): P[List[A]] =
    for {_ <- char(lhs); es <- separated(sep, pa); _ <- char(rhs)} yield es