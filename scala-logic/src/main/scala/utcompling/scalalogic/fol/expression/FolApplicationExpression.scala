package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol._
import utcompling.scalalogic.base.expression.BaseApplicationExpression

case class FolApplicationExpression(override val function: FolExpression, override val argument: FolExpression)
    extends FolExpression
    with BaseApplicationExpression[FolExpression] {

    override def visit[S](function: FolExpression => S, combinator: List[S] => S) =
        combinator(List(function(this.function), function(this.argument)))

    override def simplify(): FolExpression = {
        val function = this.function.simplify()
        val argument = this.argument.simplify()
        function match {
            case le: FolLambdaExpression => {
                le.term.replace(le.variable, argument, false, true).simplify()
            }
            case _ => {
                this.construct(List(function, argument))
            }
        }
    }

}

object FolApplicationExpression {
}
