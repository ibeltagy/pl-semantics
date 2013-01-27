package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol._
import utcompling.scalalogic.base.expression.BaseLambdaExpression

case class FolLambdaExpression(override val variable: Variable, override val term: FolExpression)
    extends FolVariableBinderExpression(FolTokens.LAMBDA, variable, term)
    with BaseLambdaExpression[FolExpression] {

}

object FolLambdaExpression {
}
