package fpa
package playground

import org.scalatest._
import scala.util.Try

class TemplateTest extends FlatSpec with Matchers {

  type Key   = String
  type Value = String
  type Env   = Map[String, String]

  def interpret(template: String)(env: Env): Try[String] = {

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
      env.getOrElse(key, abort(s"Unresolved: $key"))

    def abort(msg: String): Nothing =
      throw new IllegalArgumentException(msg)

    Try(loop(template))
  }

  "Template" should "just work" in {

    import scala.util.Success

    val env = Map(
      "c"  -> "C",
      "e"  -> "E",
      "bC" -> "BC",
      "Ef" -> "EF"
    )
    interpret("A${b${c}}D${${e}f}G")(env) should be(Success("ABCDEFG"))
  }
}
