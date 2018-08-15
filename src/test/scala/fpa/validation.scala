package fpa
package validation
package test

import java.time.LocalDate

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.{Source, StdIn}

class ValidationTests extends FlatSpec with Matchers {

  import domain._
  import validation._

  "BirthDay" should "construct from valid values" in {
    val expected = Valid(BirthDay(LocalDate.of(1987, 1, 1)))
    "01/01/1987".as[BirthDay] shouldBe expected
  }
  "BirthDay" should "construct not from valid values" in {
    val expected = Invalid(NEL.of("Invalid birthday: '01-01-1987'"))
    "01-01-1987".as[BirthDay] shouldBe expected
  }

  "Debit" should "construct from valid values" in {
    "1234567890".as[Debit] shouldBe Valid(Debit(1234567890))
    " 1234567890 ".as[Debit] shouldBe Valid(Debit(1234567890))
    "12345678.90".as[Debit] shouldBe Valid(Debit(12345678.90))
    s"${Double.MaxValue}".as[Debit] shouldBe Valid(Debit(Double.MaxValue))
  }
  "Debit" should "not construct from invalid values" in {
    "".as[Debit] shouldBe Invalid(NEL.of("Invalid debit value: ''"))
    "1,00".as[Debit] shouldBe Invalid(NEL.of("Invalid debit value: '1,00'"))
  }

  "Name" should "construct from valid values" in {
    "Last, First".as[Name] shouldBe Valid(Name("First", "Last"))
    "  Last  ,  First  ".as[Name] shouldBe Valid(Name("First", "Last"))
  }
  "Name" should "not construct from invalid values" in {
    "".as[Name] shouldBe Invalid(NEL.of("Invalid name: ''"))
    "X".as[Name] shouldBe Invalid(NEL.of("Invalid name: 'X'"))
    ",".as[Name] shouldBe Invalid(NEL.of("Invalid name: ','"))
    "X,".as[Name] shouldBe Invalid(NEL.of("Invalid name: 'X,'"))
    ",Y".as[Name] shouldBe Invalid(NEL.of("Invalid name: ',Y'"))
  }

  "AddressLine" should "construct from valid values" in {
    "Street House #1".as[Address] shouldBe Valid(Address("Street House #1"))
    "  Street House #1  ".as[Address] shouldBe Valid(Address("Street House #1"))
  }
  "AddressLine" should "not construct from invalid values" in {
    "\n\t\r ".as[Address] shouldBe Invalid(NEL.of("Empty address line"))
  }

  "PostalCode" should "construct from valid values" in {
    "1234 AB".as[PostalCode] shouldBe Valid(PostalCode("1234 AB"))
    "  1234 AB  ".as[PostalCode] shouldBe Valid(PostalCode("1234 AB"))
    "1234ab".as[PostalCode] shouldBe Valid(PostalCode("1234AB"))
    "1234 ab".as[PostalCode] shouldBe Valid(PostalCode("1234 AB"))
    "123 456".as[PostalCode] shouldBe Valid(PostalCode("123 456"))
    "  123 456  ".as[PostalCode] shouldBe Valid(PostalCode("123 456"))
  }
  "PostalCode" should "not construct from invalid values" in {
    "  ".as[PostalCode] shouldBe Invalid(NEL.of("Empty postal code"))
  }

  "Phone" should "construct from valid values" in {
    "+44123456789".as[Phone] shouldBe Valid(Phone(Some("+44"), "123456789"))
    "  +44123456789  ".as[Phone] shouldBe Valid(Phone(Some("+44"), "123456789"))
    "+44 123456789".as[Phone] shouldBe Valid(Phone(Some("+44"), "123456789"))
    "01023456789".as[Phone] shouldBe Valid(Phone(None, "01023456789"))
    "010-23456789".as[Phone] shouldBe Valid(Phone(None, "01023456789"))
    "010-234 567 89".as[Phone] shouldBe Valid(Phone(None, "01023456789"))
    "06-23456789".as[Phone] shouldBe Valid(Phone(None, "0623456789"))
    " + 4 4 1 2 3 4 5 6 7 8 9 ".as[Phone] shouldBe Valid(Phone(Some("+44"), "123456789"))
  }
  "Phone" should "not construct from invalid values" in {
    "".as[Phone] shouldBe Invalid(NEL.of("Invalid phone number: ''"))
    " ".as[Phone] shouldBe Invalid(NEL.of("Invalid phone number: ' '"))
    "#44 4567890".as[Phone] shouldBe Invalid(NEL.of("Invalid phone number: '#44 4567890'"))
  }

  "DebitRecord" should "construct from valid values" in {
    Source.fromFile("src/test/resources/data.csv").getLines().foreach { record =>
      record.as[DebitRecord] shouldBe a[Valid[_]]
    }
  }
  "DebitRecord" should "not construct from invalid values" in {
    import Fixture._
    faultyLine.as[DebitRecord] shouldBe (
      Invalid(NEL.of(s"Invalid line pattern `${RecordPattern.regex}`: '$faultyLine'"))
    )
    emptyLine.as[DebitRecord] shouldBe (
      Invalid(
        NEL.of(
          "Invalid name: ','",
          "Empty address line",
          "Empty postal code",
          "Invalid phone number: ''",
          "Invalid debit value: ''",
          "Invalid birthday: ''"
        )
      )
    )
  }
}

object Fixture {
  val testLine: String =
    """"Johnson, John",Voorstraat 32,3122gg,020 3849381,10000,01/01/1987"""
  val faultyLine: String =
    """ Last , First , Address , Code , 020 3849381 , 100 , 01/01/1987 """
  val emptyLine: String =
    """",",,,,,"""
}
