package fpa
package playground

import org.scalacheck.Gen
import org.scalatest._

import scala.util.Try

case class Template(template: String) {

  import Template._

  def interpret(env: Env): Try[String] = {

    def abort(msg: String): Nothing =
      throw new IllegalArgumentException(msg)

    def value(key: Key): Value =
      env.getOrElse(key, abort(s"Unresolved: $key"))

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

    Try(loop(template))
  }
}

object Template {
  type Key   = String
  type Value = String
  type Env   = Map[String, String]
}

class TemplateTest extends FlatSpec with Matchers {

  import scala.util.Success

  "Template.interpret(environment)" should "just work" in {

    val environment = Map("c" -> "C", "e" -> "E", "bC" -> "BC", "Ef" -> "EF" )
    val template    = Template("A${b${c}}D${${e}f}G")

    template.interpret(environment) should be(Success("ABCDEFG"))
  }
}

object TemplateProperties extends org.scalacheck.Properties("Template") {

  import org.scalacheck.Prop.forAll
  import scala.util.Success
  import Template._

  property("∀ t: String, e: Env -> Template(t).interpret(e) : Success[String]") =
    forAll { (template: String, environment: Env) =>
      Template(template).interpret(environment).isInstanceOf[Success[String]]
    }
}

