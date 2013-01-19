package utcompling.scalalogic.drt.expression.parse

import utcompling.scalalogic.drt.expression._
import org.junit.Test

class DrtLogicParserTests {

  @Test
  def test() {

    val p = new DrtLogicParser().parse(_)

    p("([],[])->([],[])").pprint()
    p("([y],[woman(y), ([x],[man(x)]) -> ([],[loves(x,y)])])").pprint()
    p("([],[-([x],[man(x)])])").pprint()

    p("\\x.P(x)").pprint()
    p("\\x.([],[man(x)])(bill)").pprint()
    p("\\x.([],[man(x)])(bill)").simplify().pprint()
    p("\\P.(P(x) -> ([],[walk(x)]))").pprint()
    println(p("\\P.(P(x) -> ([],[walk(x)]))(\\x.([x],[man(x)]))"))
    println(p("\\P.(P(x) -> ([],[walk(x)]))(\\x.([x],[man(x)]))").simplify())
    p("\\P.(P(x) -> ([],[walk(x)]))(\\x.([x],[man(x)]))").simplify().pprint()
    println(p("(\\P. \\ x. P(x))(\\y.see(y,x))").simplify())

    println(p("([x],[man(x)]) + ([y],[walk(y)])").fol)

    println(p("([y],[woman(y), ([x],[man(x)]) -> ([],[loves(x,y)])])").folModal())

    val l1 = List(
      "p:([],[])",
      "([],[P(x),p:([y],[Q(y)])])")
    for (l <- l1) {
      println(p(l))
      println(p(l) == p(p(l).toString))
      p(l).pprint()
    }

  }
}
