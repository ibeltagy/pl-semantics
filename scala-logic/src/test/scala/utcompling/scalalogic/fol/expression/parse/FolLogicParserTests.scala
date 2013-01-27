package utcompling.scalalogic.fol.expression.parse

import utcompling.scalalogic.fol.expression._
import org.junit.Test

class FolLogicParserTests {

  @Test
  def test() {

    val lp = new FolLogicParser()
    println(lp.parse("see(x,y)"))
    println(lp.parse("all x. man(x)"))
    println(lp.parse("all x. (man(x) -> exists y. woman(y) & love(x,y))"))
    println(lp.parse("(\\P.exists x.P(x))(\\y.see(y,x))").simplify())

    val e = lp.parse("(exists x0.exists x1.(john_per(w1,x0) & bill_nam(w1,x1)) & exists e2.exists p3.(((forget(w1,e2) & agent(w1,e2,x0)) & theme(w1,e2,p3)) & (R(w1,p3) & exists e4.((call(p3,e4) & agent(p3,e4,x0)) & patient(p3,e4,x1)))))")
    println(e)
    println(e.pretty)
    println()

    val c = lp.parse("((A | B) & (C & D)) & (((E -> Z) & (F & G)) & (H | (I & J)))").asInstanceOf[FolAndExpression]
    println(c.pretty)

  }
}
