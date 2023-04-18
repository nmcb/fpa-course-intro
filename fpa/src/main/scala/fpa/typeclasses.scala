package fpa
package typeclasses

object library:

  /** Type class that (optionally) masks, i.e. hides, information when disclosing it as a string. */
  trait Mask[A]:
    def disclose(a: A): String

  /** Type class companion objects contain default Mask instances */
  object Mask:

    /** A mask that masks nothing */
    def maskNothing[A]: Mask[A] =
      (a: A) => a.toString

    /** Q 3: The default should not mask at all */
    given defaultToMaskEverything[A]: Mask[A] =
      ???

  /** Q 4: We should have a nice syntax - implement it */
  extension [A : Mask](a: A) def disclose: String = ???


object module:

  import library.*

  /** Q 1: implement a client side explicit implicit parameter syntax */
  def loggingMethodUsingMaskExplicitly[A](a: A)(using mask: Mask[A]): String =
    ???

  /** Q 2: Implement a client side implicit implicit parameter syntax */
  def loggingMethodUsingMaskImplicitly[A : Mask](a: A): String =
    ???

  /** Example client-client-side usages of given functions above */
  def serviceMethodUsingLoggingMethodUsingMaskNothingMask[A](a: A): String =
    loggingMethodUsingMaskExplicitly(a)(using Mask.maskNothing)

  def serviceMethodUsingLoggingMethodUsingImplicitlyProvidedMask[A : Mask](a: A): String =
    loggingMethodUsingMaskExplicitly(a)



/** Client side code */
object Main extends App:

  import library.*

  // Client side domain
  case class BankNumber(str: String)
  case class Customer(name: String, bankNumber: BankNumber)

  object BankNumber:

    /** Q 5: BankNumbers should be masked as `BankNumber(masked)` */
    given maskBankNumber: Mask[BankNumber] =
      ???

  object Customer:
    /** Q 6: Customers should mask the contained BankNumber */
    given maskCustomer(using Mask[BankNumber]): Mask[Customer] =
      ???
