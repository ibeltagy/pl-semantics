package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import org.junit.Test

class FolExpressionTests {
  
  @Test
    def test() {

        val P = FolVariableExpression(Variable("P"))
        val Q = FolVariableExpression(Variable("Q"))
        val R = FolVariableExpression(Variable("R"))
        val x = FolVariableExpression(Variable("x"))
        val y = FolVariableExpression(Variable("y"))
        val z = FolVariableExpression(Variable("z"))

        println(P(x, y, z))

        println(-P(x))
        println(P(x) & Q(x))
        println(P(x) | Q(x))
        println(P(x) -> Q(x))
        println(P(x) <-> Q(x))
        println(P(x) & Q(x) & R(x))
        println(P(x) all x.variable)
        println(P(x) exists x.variable)
        println(P(x) lambda x.variable)

        val a = (P(x) & Q(x, y, z) & R(x, z))
        println(a.free())
        println(a.constants())
        println(a.predicates())

        println(P.construct(List(Q.variable)))

        val c = P(x) & Q(y)
        println(c.replace(x.variable, z))
        println(c.normalize())
        
    }
}
