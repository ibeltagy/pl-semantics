package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerEqv(discId: String, indices: List[BoxerIndex], first: BoxerExpression, second: BoxerExpression) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(List(function(first), function(second)))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerEqv(discId, indices, function(first), function(second))

  override def toString() =
    "[%s]:eqv(%s,%s)".format(indices.mkString(","), first, second)

}

object BoxerEqv {

}