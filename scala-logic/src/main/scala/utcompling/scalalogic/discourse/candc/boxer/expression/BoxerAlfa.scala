package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerAlfa(variable: BoxerVariable, first: BoxerExpression, second: BoxerExpression) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(List(function(first), function(second)))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerAlfa(variable, function(first), function(second))

  override def toString(): String =
    "alfa(%s,%s,%s)".format(variable.name, first, second)

}

object BoxerAlfa {

}