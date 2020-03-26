//object prolog {
//
//  type Unknown  = String
//  type Constant = String
//
//  sealed trait Term
//  case class Var(unknown: Unknown) extends Term
//  case class Fun(constant: Constant, terms: List[Term]) extends Term
//  case class Law(term: Term, given: List[Term]) extends Term
//
//
//  type Subst = Map[Unknown, Term]
//  val empty: Subst =
//    Map.empty
//
//  def add(unknown: Unknown)(term: Term)(subst: Subst): Subst =
//    subst + (unknown -> term)
//
//  type Attempt = Option[Subst]
//  def unify(lhs: Term, rhs: Term)(attempt: Attempt): Attempt = {
//
//    def uni(lhs: Term, rhs: Term)(subst: Subst): Subst =
//      ???
//
//    attempt match {
//      case None => None
//      case Some(subst) => uni(subst(lhs), subst(rhs))
//    }
//  }
//}
//


