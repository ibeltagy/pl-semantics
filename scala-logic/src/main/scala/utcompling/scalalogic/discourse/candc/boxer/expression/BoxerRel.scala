package utcompling.scalalogic.discourse.candc.boxer.expression

import utcompling.scalalogic.top.expression.Variable

case class BoxerRel(discId: String, indices: List[BoxerIndex], event: BoxerVariable, variable: BoxerVariable, name: String, sense: Int) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    default

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerRel(discId, indices, function(event).asInstanceOf[BoxerVariable], 
        function(variable).asInstanceOf[BoxerVariable], name, sense)

  override def toString(): String =
    "[%s]:rel(%s,%s,%s,%d)".format(indices.mkString(","), event.name, variable.name, name, sense)

}

object BoxerRel {

}