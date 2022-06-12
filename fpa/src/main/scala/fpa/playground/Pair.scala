package fpa
package playground

case class AllInOnePair[T0,T1](t0: T0, t1: T1)(implicit ordT0: Ordering[T0], ordT1: Ordering[T1]) {
  def `===`(that: AllInOnePair[T0,T1]): Boolean =
    (this.t0 == that.t0) && (this.t1 == that.t1)
  def `__>`(that: AllInOnePair[T0,T1]): Boolean = {
    val ot0 = ordT0.compare(this.t0, that.t0)
    val ot1 = ordT1.compare(this.t1, that.t1)
    (ot0, ot1) match {
      case (-1,  _) => false // left biased
      case ( 0, -1) => false
      case ( 0,  0) => false
      case ( _,  1) => true
    }
  }

  def `_>=`(that: AllInOnePair[T0,T1]): Boolean =
     this.`===`(that)  ||  this.`__>`(that)
  def `__<`(that: AllInOnePair[T0,T1]): Boolean =
    !this.`===`(that)  && !this.`__>`(that)
  def `_<=`(that: AllInOnePair[T0,T1]): Boolean =
     this.`===`(that)  && !this.`__>`(that)
}

object AllInOnePair extends App {
  val p1 = AllInOnePair(1, "one")
  val p2 = AllInOnePair(2, "two")
  val p0 = AllInOnePair(0,  1.0 )

  // println(p0 `===` p1)  // type error
  println(p1 `===` p2)
  println(p1 `__>` p2)
  println(p1 `_>=` p2)
  println(p1 `__<` p2)
  println(p1 `_<=` p2)
}

case class JustTheDataPair[T0,T1](t0: T0, t1: T1)

object JustTheDataPair extends App {
  implicit def ordPair[T0 : Ordering,T1 : Ordering]: Ordering[JustTheDataPair[T0,T1]] = {
    val ordT0 = implicitly[Ordering[T0]]
    val ordT1 = implicitly[Ordering[T1]]
    (p0: JustTheDataPair[T0, T1], p1: JustTheDataPair[T0, T1]) => {
      val ot0 = implicitly[Ordering[T0]].compare(p0.t0, p1.t0)
      val ot1 = implicitly[Ordering[T1]].compare(p1.t1, p1.t1)
      (ot0, ot1) match {
        case (-1,  _) => -1 // left biased
        case ( 0, -1) => -1
        case ( 0,  0) =>  0
        case ( _,  1) =>  1
      }
    }
  }

  implicit class AllInOnePairOps[T0,T1](self: AllInOnePair[T0,T1])(implicit ord: Ordering[AllInOnePair[T0,T1]]) {
    def `===`(that: AllInOnePair[T0,T1]): Boolean =
      (self.t0 == that.t0) && (self.t1 == that.t1)
    def `__>`(that: AllInOnePair[T0,T1]): Boolean =
      ord.gt(self, that)

    def `_>=`(that: AllInOnePair[T0,T1]): Boolean =
      ord.gteq(self, that)
    def `__<`(that: AllInOnePair[T0,T1]): Boolean =
      ord.lt(self, that)
    def `_<=`(that: AllInOnePair[T0,T1]): Boolean =
      ord.lteq(self, that)
  }

  val p1 = AllInOnePair(1, "one")
  val p2 = AllInOnePair(2, "two")
  val p0 = AllInOnePair(0,  1.0 )

  // println(p0 `===` p1)  // type error
  println(p1 `===` p2)
  println(p1 `__>` p2)
  println(p1 `_>=` p2)
  println(p1 `__<` p2)
  println(p1 `_<=` p2)
}