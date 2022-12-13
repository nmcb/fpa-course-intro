package fpa
package typeclasses
package tests

import org.scalatest.*
import flatspec.*
import matchers.*

class MaskSpec extends AnyFlatSpec {

  import library.*

  "The Mask type class" should "default to disclosing everything" in {
    import java.util.UUID

    summon[Mask[String]].disclose("<any-string>") === "<any-string>"
    summon[Mask[Int]].disclose(666)               === "666"
    summon[Mask[Double]].disclose(6.6)            === "6.6"

    val UUIDPattern = "([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})".r
    summon[Mask[UUID]].disclose(UUID.randomUUID()) match {
      case UUIDPattern(_) => // success
      case str: String    => fail(s"Expected to match: ${UUIDPattern.regex} but was: $str")
    }
  }

  it should "not disclose client bank numbers" in {
    import Main.*
    val bankNumber = BankNumber("1234567890")
    summon[Mask[BankNumber]].disclose(bankNumber) === "BankNumber(masked)"
  }

  it should "not disclose bank numbers used as case class attributes" in {
    import Main.*
    val customer = Customer("Name", BankNumber("1234567890"))
    summon[Mask[Customer]].disclose(customer) === "Customer(Name,BankNumber(masked))"
  }

  it should "have a nice syntax" in {
    import Main.*
    assertCompiles ("""Customer("Name", BankNumber("1234567890")).disclose""")
    Customer("Name", BankNumber("1234567890")).disclose === "Customer(Name,BankNumber(masked))"
  }
}
