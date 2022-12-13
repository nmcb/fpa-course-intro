package fpa
package typeclasses

object library {

  /** Type class that (optionally) masks, i.e. hides, information when disclosing it as a string. */
  trait Mask[A] {
    def disclose(a: A): String
  }

  /** Q 1: implement a client side explicit implicit parameter syntax */
  def loggingMethodUsingMaskExplicitly[A](a: A)(using mask: Mask[A]): String =
    ???

  /** Q 2: Implement a client side implicit implicit parameter syntax */
  def loggingMethodUsingMaskImplicitly[A : Mask](a: A): String =
    ???

  /** Example client-client-side usages of given functions above */
//  def serviceMethodUsingLoggingMethodUsingMaskNothingMask[A](a: A): String =
//    loggingMethodUsingMaskExplicitly(a)(Mask.maskNothing)

  def serviceMethodUsingLoggingMethodUsingImplicitlyProvidedMask[A : Mask](a: A): String =
    loggingMethodUsingMaskExplicitly(a)


  /** Type class companion objects contain default Mask instances */
  object Mask:

    /** A mask that masks nothing */
    def maskNothing[A]: Mask[A] =
      (a: A) => a.toString

    /** Q 3: The default should not disclose anything at all */
    given defaultToNoMask[A]: Mask[A] =
      (a: A) => maskNothing.disclose(a)


  /** Q 4: We should have a nice syntax - implement it */
  implicit class MaskOps[A : Mask](a: A) {
    def disclose: String =
      ???
  }
}

/** Client side code */
object Main extends App {

  import library.*

  // Client side domain
  case class BankNumber(str: String)
  case class Customer(name: String, bankNumber: BankNumber)
  case class Laptop(frequency: Long)

  object Laptop:
    given Mask[Laptop] = Mask.maskNothing

  val laptop = Laptop(1000)
  serviceMethodUsingLoggingMethodUsingImplicitlyProvidedMask(laptop)

  object BankNumber:
    /** Q 5: BankNumbers should be masked as `BankNumber(masked)` */
    given Mask[BankNumber] = (_: BankNumber) => "BankNumber(masked)"

  object Customer {
    /** Q 6: Customers should mask the contained BankNumber */
    given maskCustomer(using bankNumberMask: Mask[BankNumber]): Mask[Customer] =
      (c: Customer) => s"Customer(${c.name}, ${bankNumberMask.disclose(c.bankNumber)})"
  }
}