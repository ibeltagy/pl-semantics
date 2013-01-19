package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.base.expression.BaseAtom

object FolAtom extends BaseAtom[FolApplicationExpression, FolExpression] {

    override protected def makeVariableExpression(v: Variable) =
        FolVariableExpression(v)

}
