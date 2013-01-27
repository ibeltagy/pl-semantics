package utcompling.scalalogic.base.expression

import utcompling.scalalogic.base.Tokens

import utcompling.scalalogic.top.expression.Variable

trait BaseBinaryExpression[T <: BaseExpression[T]] extends BaseExpression[T] {

    val first: T
    val second: T
    val operator: String

    override def visit[S](function: T => S, combinator: List[S] => S) =
        combinator(List(function(this.first), function(this.second)))

    override def toString() =
        Tokens.OPEN + first + " " + this.operator + " " + second + Tokens.CLOSE

    def getAllConnectedSameLevel(): List[T] = {
        def collect(cur: BaseExpression[T]): List[BaseExpression[T]] =
            cur match {
                case e: BaseBinaryExpression[T] if (e.operator == this.operator) => collect(e.first) ++ collect(e.second)
                case _ => List(cur)
            }
        return collect(this).asInstanceOf[List[T]]
    }

}