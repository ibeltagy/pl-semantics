package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerMerge(pred: String, first: BoxerExpression, second: BoxerExpression) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(List(function(first), function(second)))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerMerge(pred, function(first), function(second))

  override def toString(): String =
    "%s(%s,%s)".format(pred, first, second)

}

object BoxerMerge {

}