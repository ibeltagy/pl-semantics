package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol._
import utcompling.scalalogic.base.expression.BaseVariableExpression

case class FolVariableExpression(override val variable: Variable)
    extends FolExpression
    with BaseVariableExpression[FolExpression] {

}

object FolVariableExpression {
}
