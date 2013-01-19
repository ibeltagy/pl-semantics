package utcompling.scalalogic.drt

import utcompling.scalalogic.drt._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.drt.expression._
import org.junit.Test

class Tests {

  @Test
  def test() {

    DrtBoxExpression(List(Variable("y")),
      List(DrtApplicationExpression(DrtVariableExpression(Variable("woman")),
        DrtVariableExpression(Variable("x"))))).pprint

  }
}
