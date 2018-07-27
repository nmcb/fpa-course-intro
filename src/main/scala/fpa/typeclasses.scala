package fpa
package typeclasses

object library {

  /** Type class that (optionally) masks, i.e. hides, information when disclosing it as a string. */
  trait Mask[A] {
    def disclose(a: A): String
  }

  /** Q 1: implement a client side explicit implicit parameter syntax */
  def methodUsingMaskExplicitly[A](a: A)(implicit mask: Mask[A]): String =
    mask.disclose(a)

  /** Q 2: Implement a client side implicit implicit parameter syntax */
  def methodUsingMaskImplicitly[A : Mask](a: A): String =
    implicitly[Mask[A]].disclose(a)


  /** Type class companion objects contain default Mask instances */
  object Mask {
    /** Q 3: The default should not to mask at all */
    implicit def noMask[A]: Mask[A] =
      (a: A) => a.toString
  }

  /** Q 4: We should have a nice syntax */
  implicit class MaskSyntax[A : Mask](a: A) {
    def disclose: String =
      implicitly[Mask[A]].disclose(a)
  }
}

/** Client side code */
object client extends App {

  import library._

  // Client side domain
  case class BankNumber(str: String)
  object BankNumber {
    /** Q 5: BankNumbers should be masked as `BankNumber(masked)` */
    implicit def maskBankNumber: Mask[BankNumber] = {
      _ => "BankNumber(masked)"
    }
  }

  case class Customer(name: String, bankNumber: BankNumber)
  object Customer {
    /** Q 6: Customers should mask the contained BankNumber */
    implicit def maskCustomer(implicit bankNumberMask: Mask[BankNumber]): Mask[Customer] =
      (customer: Customer) =>
        s"Customer(${customer.name.disclose},${customer.bankNumber.disclose})"
  }
}