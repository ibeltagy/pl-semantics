package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerImp(discId: String, indices: List[BoxerIndex], first: BoxerExpression, second: BoxerExpression) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(List(function(first), function(second)))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerImp(discId, indices, function(first), function(second))

  override def toString() =
    "[%s]:imp(%s,%s)".format(indices.mkString(","), first, second)

}

object BoxerImp {

}