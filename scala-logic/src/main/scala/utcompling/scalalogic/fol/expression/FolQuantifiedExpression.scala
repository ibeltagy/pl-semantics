package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol._

abstract class FolQuantifiedExpression(override val operator: String, override val variable: Variable, override val term: FolExpression)
    extends FolVariableBinderExpression(operator, variable, term) {

}
