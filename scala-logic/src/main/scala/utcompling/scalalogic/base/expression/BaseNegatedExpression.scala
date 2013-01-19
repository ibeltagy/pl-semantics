package utcompling.scalalogic.base.expression

trait BaseNegatedExpression[T <: BaseExpression[T]] extends BaseExpression[T] {

    val term: T

    override def visit[S](function: T => S, combinator: List[S] => S) =
        combinator(List(function(this.term)))

}