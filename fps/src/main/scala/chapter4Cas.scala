package fps
package cas

sealed trait Option[+A] {

  def map[B](f: A => B) : Option[B] = 
    this match
      case None => None
      case Some(a) => Some(f(a))
  

  def flatMap[B](f: A => Option[B]) : Option[B] = 
    this match
      case None => None
      case Some(a) => f(a)
  

  def getOrElse[B >: A](default: => B) : B = 
    this match 
      case None => default
      case Some(a) => a
  

  def orElse[B >: A](ob: => Option[B]) : Option[B] = 
    this match
      case None => ob
      case _ => this
  

  def filter(f: A => Boolean) : Option[A] = 
    this match 
      case Some(a) if f(a) => this
      case _ => None
}  

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]


def failingFn(i: Int): Int = 
  val y: Int = throw new Exception("fail!")
  try {
    val x = 42 + 5
    x + y
  }
  catch { case e: Exception => 43 }


def failingFn2(i: Int): Int = 
  try {
    val x = 42 + 5
    x + ((throw new Exception("fail!")): Int)
  }
  catch { case e: Exception => 43 }

  
def mean(xs: Seq[Double]): Option[Double] = 
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)


def variance(xs: Seq[Double]) : Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  
// mean(xs) gives option of some or none, where some is the mean of xs
// flatmap over that to get access to the content of the option
// xs.map maps over the current sequence and for each element does the function
// get the mean over those squared deviations

// def parseInsuranceRateQuote(age: String, numOfTick: String) : Option[Double] =
//   val optAge: Option[Int] = Try(age.toInt)
//   val optTickets: Option[Int] = Try(numOfTick.toInt)
//   insuranceRateQuote(optAge, optTickets)

def Try[A] (a: => A) : Option[A] =
  try Some(a)
  catch { case e: Exception => None }


def map2_pat[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] =
  (a, b) match
    case (Some(a), Some(b)) => Some(f(a,b))
    case _ => None

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] =
  a.flatMap(aa => b.map(bb => f(aa,bb)))    

// def sequence[A] (a: List[Option[A]]) : Option[List[A]] =
//   a match
//     case (h,t) => 
//     case 

def sequence[A](a: List[Option[A]]): Option[List[A]] = 
  a match
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))


object Main2 extends App:
  // println(failingFn(12))
  // println(failingFn2(12))

  val testSome = List(1,2,3)
  val testLoneSome = Some(666)
  val testLoneNone : Option[Nothing] = None

  val testListSome : List[Option[Int]] = List(Some(4), Some(5), Some(6))

  val testSomeMap = Map(1 -> "Boo", 2 -> "BooBoo") // doesnt work with our implementation, but helps me understand

  println(testLoneSome.map(x => Some(x + 1)))
  
  println(sequence(testListSome))
  // println(testSome.map(a => a + 1)) // apply function to each element in list
  // println(testLoneSome.getOrElse(0)) // on some(666) so it gets the value
  // println(testLoneNone.getOrElse(0)) // this works on none, so it returns the default value
  // println(testSomeMap.getOrElse(2, "NoBoo"))
  // println(testSomeMap.getOrElse(3, "NoBoo")) // not in map, so returns default

  // println(testLoneSome.orElse(None)) // i don't get how to use this one


  // val testSeq = Seq(1.0, 2.0, 3.0, 4.0)
  // println(variance(testSeq))