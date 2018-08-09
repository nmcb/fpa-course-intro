package fpa
package typeclasses
package tests

import java.util.UUID

import org.scalatest._
import library._

//@Ignore
//class MaskSpec extends FlatSpec with Matchers {
//
//  "The Mask type class" should "default to disclosing everything" in {
//    implicitly[Mask[String]].disclose("<any-string>") should be ("<any-string>")
//    implicitly[Mask[Int]].disclose(666) should be ("666")
//    implicitly[Mask[Double]].disclose(6.6) should be ("6.6")
//
//    val UUIDPattern = "([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})".r
//    implicitly[Mask[UUID]].disclose(UUID.randomUUID()) match {
//      case UUIDPattern(uuid) => // success
//      case str: String       => fail(s"Expected to match: ${UUIDPattern.regex} but was: $str")
//    }
//  }
//
//  it should "not disclose client bank numbers" in {
//    import client._
//    val bankNumber = BankNumber("1234567890")
//    implicitly[Mask[BankNumber]].disclose(bankNumber) should be ("BankNumber(masked)")
//  }
//
//  it should "not disclose bank numbers used as case class attributes" in {
//    import client._
//    val customer = Customer("Name", BankNumber("1234567890"))
//    implicitly[Mask[Customer]].disclose(customer)  should be ("Customer(Name,BankNumber(masked))")
//  }
//
//  it should "have a nice syntax" in {
//    import client._
//    """Customer("Name", BankNumber("1234567890")).disclose""" should compile
//    Customer("Name", BankNumber("1234567890")).disclose should be ("Customer(Name,BankNumber(masked))")
//  }
//}

object MaskSpec