package utcompling.scalalogic.discourse.candc.boxer.expression

import utcompling.scalalogic.top.expression.Variable

case class BoxerProp(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, drs: BoxerExpression) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(List(function(drs)))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerProp(discId, indices, function(variable).asInstanceOf[BoxerVariable], function(drs))

  override def toString() =
    "[%s]:prop(%s,%s)".format(indices.mkString(","), variable.name, drs)

}

object BoxerProp {

}