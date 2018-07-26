package fpa
package typeclasses

object library {

  /** Type class that (optionally) masks, i.e. hides, information when disclosing it as a string. */
  trait Mask[A] {
    def disclose(a: A): String
  }

  /** Type class companion objects contain default Mask instances */
  object Mask {
    /** Q: The default should not to mask at all */
    implicit def noMask[A]: Mask[A] =
      ???
  }

  /** Q: We should have nice syntax */
}

/** Client side code */
object client extends App {

  import library._

  /** Q: implement a client side explicit implicit parameter syntax */
  def methodUsingMaskExplicitly[A](a: A)(implicit mask: Mask[A]): String =
    ???

  /** Q: Implement a client side implicit implicit parameter syntax */
  def methodUsingMaskImplicitly[A : Mask](a: A): String =
    ???

  // Client side domain
  case class BankNumber(str: String)
  object BankNumber {
    /** BankNumbers should be masked as `BankNumber(masked)` */
    implicit def maskBankNumber: Mask[BankNumber] =
      ???
  }
  case class Customer(name: String, bankNumber: BankNumber)
  object Customer {
    /** Customers should mask the contained BankNumber */
    implicit def maskCustomer(implicit bankNumberMask: Mask[BankNumber]): Mask[Customer] =
      ???
  }

}