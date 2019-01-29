package fpa
package playground
package test

object UUIDSpec extends org.scalacheck.Properties("UUID") {

  import org.scalacheck._
  import org.scalacheck.Prop.forAll

  // Generates a version 4 (random) UUID -- TODO Falsify
  implicit val _java_util_UUID_version_4: Gen[java.util.UUID] =
    for {
      l1 <- Gen.choose(Long.MinValue, Long.MaxValue)
      l2 <- Gen.choose(Long.MinValue, Long.MaxValue)
      y  <- Gen.oneOf('8', '9', 'a', 'b')
    } yield java.util.UUID.fromString(
      new java.util.UUID(l1,l2).toString.updated(14, '4').updated(19, y)
    )

  property(
    s"""parser.parseUUID : ∀ uuid ∈ java.util.UUID ->
       |  parseUUID(uuid.toString).msb === uuid.msb
       |  parseUUID(uuid.toString).lsb === uuid.lsb
       |  """.stripMargin) =
    forAll { (uuid: java.util.UUID) =>
        UUID.parser.parseUUID(uuid.toString).msb == uuid.getMostSignificantBits &&
        UUID.parser.parseUUID(uuid.toString).lsb == uuid.getLeastSignificantBits
    }
}
