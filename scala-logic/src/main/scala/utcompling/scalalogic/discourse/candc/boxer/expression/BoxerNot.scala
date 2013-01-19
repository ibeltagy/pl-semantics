package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerNot(discId: String, indices: List[BoxerIndex], drs: BoxerExpression) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(List(function(drs)))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerNot(discId, indices, function(drs))

  def ::(index: Int) = BoxerNot(discId, List(BoxerIndex(index)), drs)

  override def toString() =
    "[%s]:not(%s)".format(indices.mkString(","), drs)

}

object BoxerNot {

}