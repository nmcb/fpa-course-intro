package fpa
package properties
package test

import org.scalacheck._

object PropertiesSpec extends Properties("Properties") {

import Prop._

/** Q1: Implement the property test for ... */
// property("list == list.reverse.reverse") =
//   ???

/** Q2: Implement a more precise property to show that ... */
// property("list.head == list.reverse.last") =
//   ???

/** Q3: Implement the property test for ... */
// property("plus on doubles is associative") =
//   ???

/** Q4: See if you can devise a more precise property that holds for ... */
// property("times and division on doubles are each others complement") =
//   ???
 

/** This may go wrong when using doubles to represent money, so let's define ... */
type Money = BigDecimal
object Money {
  val Zero = BigDecimal(0)
}

/** We can create value generators from the `Gen._` utility functions */
val genLessThanOneMillionEuro: Gen[Money] =
  Gen.choose[Int](0, 99999999).map(i => BigDecimal(i) / 100)

/** Q5: See if you can devise a more precise property that holds for ... */
// property("times and division on big decimals < 1000000 && >= 0 are each others complement") =
//   ???

case class Product(name: String, price: Money)
case class Order(lines: List[Product]) {
  def total: Money = lines.map(_.price).foldLeft(Money.Zero)(_+_)
}

/** Generators can be composed */
val genProducts: Gen[Product] = for {
  name  <- Gen.asciiPrintableStr
  price <- genLessThanOneMillionEuro
} yield Product(name, price)

/** Q6: Implement the property that ... */
// property("some order total == the sum of the product line prices")  =
//   ???

/** Generators can also easily aggregate know failure scenario's, e.g. for ... */
object Dates {
  val dateLiteral =
    """([0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])""".r
  def isValidDateLiteral(repr: String) =
    dateLiteral.findFirstIn(repr).isDefined
}

/** Known invalid value literals that violate ISO-8601's YYYY-MM-DD format */
val invalidDateLiterals: Gen[String] = Gen.oneOf[String]("date",
  "0000-00-00", "0000-13-01", "0000-01-32", "2016-1-1", "-1601-01", "2016-31-12",
  "02/27/2013", "02/27/13", "27/02/2013", "27/02/13", "20130227", "2013.02.27",
  "27.02.13", "27-02-13", "27.2.13", "2013.II.27", "27/2-13", "2013.158904109",
  "MMXIII-II-XXVII", "MMXIII LVII/CCCLXV", "1330300800", "", "0", "1234567890"
)

// property("rejects invalid date literals, i.e. other than the `YYYY-MM-DD` format") =
//   ???
}
