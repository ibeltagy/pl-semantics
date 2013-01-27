package scalalogic

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.util.Counter
import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.fol.expression._
import org.junit.Test

class Tests {

  @Test
  def test() {

    val vP = Variable("P")
    println(vP)
    val P = FolVariableExpression(vP)
    println(P)
    val Q = FolVariableExpression(Variable("Q"))
    println(Q)
    val PnQ = FolAndExpression(P, Q)
    println(PnQ)
    val lP_PnQ = FolLambdaExpression(vP, PnQ)
    println(lP_PnQ)
    val aP_PnQ = FolAllExpression(vP, PnQ)
    println(aP_PnQ)
    val nP = FolNegatedExpression(P)
    println(nP)

    println(-P)
    println(P & Q)

    println(Variable.unique(Variable("x")))
    println(Variable.unique(Variable("x")))

    val counter = new Counter
    println(counter.get)
    println(counter.get)


    val lp = new FolLogicParser()
    println(lp.parse("all x. man(x)"))
    println(lp.parse("all x. (man(x) -> exists y. woman(y) & love(x,y))"))

  }
}
