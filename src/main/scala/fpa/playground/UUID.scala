package fpa.playground

case class UUID(msb: Long, lsb: Long)

object UUID {

  object parser {

    import fastparse._
    import fastparse.Parsed.{Failure, Success}
    import NoWhitespace._

    def hexDigit[_: P]: P[Unit]            = CharIn("0-9a-fA-F")
    def hexOctet[_: P]: P[Unit]            = hexDigit ~ hexDigit
    def node[_: P]: P[Unit]                = hexOctet ~ hexOctet ~ hexOctet ~ hexOctet ~ hexOctet ~ hexOctet
    def clockSeqLow[_: P]: P[Unit]         = hexOctet
    def clockSeqAndReserved[_: P]: P[Unit] = hexOctet
    def timeHighAndVersion[_: P]: P[Unit]  = hexOctet ~ hexOctet
    def timeMid[_: P]: P[Unit]             = hexOctet ~ hexOctet
    def timeLow[_: P]: P[Unit]             = hexOctet ~ hexOctet ~ hexOctet ~ hexOctet
    def uuid[_: P]: P[UUID]                = (
                                               timeLow ~ "-" ~ timeMid ~ "-" ~
                                               timeHighAndVersion ~ "-" ~
                                               clockSeqAndReserved ~
                                               clockSeqLow ~ "-" ~ node
                                             ).!
                                              .map(java.util.UUID.fromString)
                                              .map(uuid => UUID(uuid.getMostSignificantBits, uuid.getLeastSignificantBits))

    def parseUUID(s: String): UUID = parse(s, uuid(_)) match {
      case Success(v, _) => v
      case f: Failure    => { println(f) ; sys.error(f.msg) }
    }
  }
}
