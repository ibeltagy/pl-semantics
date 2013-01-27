package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.base.expression.BaseAtom

object DrtAtom extends BaseAtom[DrtApplicationExpression, DrtExpression] {

    override protected def makeVariableExpression(v: Variable) =
        DrtVariableExpression(v)

}