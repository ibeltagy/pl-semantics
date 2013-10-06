package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerEq(discId: String, indices: List[BoxerIndex], first: BoxerVariable, second: BoxerVariable) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    default

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerEq(discId, indices, function(first).asInstanceOf[BoxerVariable], function(second).asInstanceOf[BoxerVariable])

  override def toString(): String =
    "[%s]:eq(%s,%s)".format(indices.mkString(","), first.name, second.name)

}

object BoxerEq {

}