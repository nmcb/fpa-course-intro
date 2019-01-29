package fpa
package fpa.playground.agda

object lite {

  sealed trait Expr
  sealed trait Term extends Expr
  sealed trait Type extends Expr
  case class Var(symbol: Symbol)           extends Term  // Variables
  case class App(u: Term, v: Term)         extends Term  // Application
  case class Lam(x: Var, u: Expr)          extends Term  // Lambda
  case class Dep(x: Var, a: Type, b: Type) extends Type  // Dependent function type --TODO should be function type
  case class Set(level: Int)               extends Term  // Universe
  case class Dtp(symbol: Symbol)           extends Type  // Datatype
  case class Dat(symbol: Symbol, t: Type)  extends Type  // Data constructor
  case class Fun(symbol: Symbol)           extends Term  // Defined function

}