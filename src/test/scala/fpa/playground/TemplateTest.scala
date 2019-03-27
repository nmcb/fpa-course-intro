package fpa
package playground

import org.scalatest._
import scala.util.Try

class TemplateTest extends FlatSpec with Matchers {

  type Key   = String
  type Value = String
  type Vars  = Map[String, String]

  def interpret(template: String)(environment: Vars): Try[String] = {

    @scala.annotation.tailrec def loop(cur: String, acc: String = "", keys: List[Key] = Nil): String =
      keys match {
        case k :: Nil     if cur.startsWith("}")  => loop( cur.tail    , acc + value(k) , Nil                )
        case k :: p :: ks if cur.startsWith("}")  => loop( cur.tail    , acc            , p + value(k) :: ks )
        case _            if cur.startsWith("${") => loop( cur.drop(2) , acc            , "" :: keys         )
        case k :: ks      if cur.nonEmpty         => loop( cur.tail    , acc            , k + cur.head :: ks )
        case Nil          if cur.nonEmpty         => loop( cur.tail    , acc + cur.head , keys               )
        case Nil                                  => acc
        case _                                    => abort(s"Unclosed bracket on: $${${keys.head}")
      }

    def value(key: Key): Value =
      environment.getOrElse(key, abort(s"Unresolved: $key"))

    def abort(msg: String): Nothing =
      throw new IllegalArgumentException(msg)

    Try(loop(template))
  }

  "Template" should "just work" in {

    import scala.util.Success

    val env = Map(
      "c"   -> "C",
      "g"   -> "G",
      "bCd" -> "BCD",
      "fGh" -> "FGH"
    )
    interpret("A${b${c}d}E${f${g}h}I")(env) should be(Success("ABCDEFGHI"))
  }
}
