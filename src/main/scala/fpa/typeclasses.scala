package fpa
package typeclasses

object library {

  /** Type class that (optionally) masks, i.e. hides, information when disclosing it as a string. */
  trait Mask[A] {
    def disclose(a: A): String
  }

  /** Type class companion object containing default Mask instances */
  object Mask {

    /** The default is not to mask at all */
    implicit def noMask[A]: Mask[A] = new Mask[A] {
      def disclose(a: A): String = a.toString
    }
  }

  implicit class MaskSyntax[A : Mask](a: A) {
    val disclose: String =
      implicitly[Mask[A]].disclose(a)
  }
}

/** Client side code */
object client extends App {

  import library._

  // Client side explicit implicit parameter syntax
  def methodUsingMaskExplicitly[A](a: A)(implicit mask: Mask[A]): String =
    mask.disclose(a)

  // Client side implicit implicit parameter syntax
  def methodUsingMaskImplicitly[A : Mask](a: A): String =
    implicitly[Mask[A]].disclose(a)

  // Client side domain
  case class BankNumber(str: String)
  object BankNumber {
    implicit val maskBankNumber: Mask[BankNumber] = new Mask[BankNumber] {
      override def disclose(a: BankNumber): String =
        "BankNumber(masked)"
    }
  }
  case class Customer(name: String, bankNumber: BankNumber)
  object Customer {
    implicit def maskCustomer(implicit bankNumberMask: Mask[BankNumber]): Mask[Customer] = new Mask[Customer] {
      override def disclose(customer: Customer): String =
        s"Customer(${customer.name},${bankNumberMask.disclose(customer.bankNumber)})"
    }
  }

}