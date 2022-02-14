object prolog {

  type UpperCase = String
  type LowerCase = String
  type Tag       = String

  sealed trait Term
  case class Var(name: UpperCase)                    extends Term
  case class Fun(name: LowerCase, terms: List[Term]) extends Term

  type TaggedTerm = (Tag, Term)

  case class Rule(lhs: Term, rhs: List[Term])

  trait Taggable[A] {
    def tag(t: Tag)(a: A): A
  }

  implicit def taggableList[A : Taggable]: Taggable[List[A]] = new Taggable[List[A]] {
    override def tag(t: Tag)(as: List[A]): List[A] =
      as.map(implicitly.tag(t))
  }

  implicit def taggableTerm(implicit T: Taggable[List[Term]]): Taggable[Term] = new Taggable[Term] {
    override def tag(t: Tag)(term: Term): Term = term match {
      case Var(name)        => Var(name + t)
      case Fun(name, terms) => Fun(name, T.tag(t)(terms))
    }
  }

  implicit def taggablerule(implicit T: Taggable[List[Term]]): Taggable[Rule] = new Taggable[Rule] {
    override def tag(t: Tag)(rule: Rule): Rule = Rule(rule.lhs, T.tag(t)(rule.rhs))
  }

  case class Env(toMap: Map[UpperCase, Term])

  def emptyEnv: Option[Env] =
    Some(Env(Map.empty))

  type ApplyRule = (Tag, Rule, Result)

  sealed trait Result
  case class Done(env: Env) extends Result
  case class ApplyRules(rules: List[ApplyRule]) extends Result

  type Proofs = List[(Tag, Rule)]

  trait Subst[A] {
    def subst(env: Env)(a: A): A
  }

  implicit def substList[A : Subst]: Subst[List[A]] = new Subst[List[A]] {
    override def subst(env: Env)(as: List[A]): List[A] =
      as.map(implicitly subst env)
  }

  implicit def substTerm(implicit L: Subst[List[Term]]): Subst[Term] = new Subst[Term] {
    override def subst(env: Env)(term: Term): Term =
      term match {
        case Var(name)        => env.toMap.getOrElse(name, Var(name))
        case Fun(name, terms) => Fun(name, L.subst(env)(terms))
      }
  }

  implicit def substRule(implicit L: Subst[List[Term]], T: Subst[Term]): Subst[Rule] = new Subst[Rule] {
    override def subst(env: Env)(rule: Rule): Rule =
      Rule(T.subst(env)(rule.lhs),  L.subst(env)(rule.rhs))
  }

  def unify(lhs: Term, rhs: Term)(acc: Option[Env])(implicit T: Subst[Term]): Option[Env] =

    acc match {
      case None =>
        Option.empty[Env]
      case env@Some(cur) =>
        (T.subst(cur)(lhs), T.subst(cur)(rhs)) match {
          case (Var(x), y) =>
            Some(Env(cur.toMap + (x -> y)))
          case (x, Var(y)) =>
            Some(Env(cur.toMap + (y -> x)))
          case (Fun(x, xs), Fun(y, ys)) if x == y && xs.length == ys.length =>
            xs.zip(ys).foldRight[Option[Env]](env) { (q, w) => unify(q._1, q._2)(w)(T) }
          case _ =>
            None
        }
    }

  def solve(rules: List[Rule], env: Option[Env], tags: List[TaggedTerm])(implicit L: Taggable[List[Rule]], T: Subst[Term]): Result =
    (rules, env, tags) match {
      case (_, None, _) =>
        ApplyRules(List.empty[ApplyRule])
      case (_, Some(e), Nil) =>
        Done(e)
      case (rs, e, (tg, t) :: ts) =>
        ???
    }

}



