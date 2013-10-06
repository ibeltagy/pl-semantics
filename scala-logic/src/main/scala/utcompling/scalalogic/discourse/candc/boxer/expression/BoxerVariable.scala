package utcompling.scalalogic.discourse.candc.boxer.expression

import utcompling.scalalogic.top.expression.Variable

case class BoxerVariable(name: String) extends BoxerExpression with Ordered[BoxerVariable] {

  def compare(that: BoxerVariable): Int =
    this.name.compare(that.name)

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    sys.error("BoxerVariable.visit() is not implemented")

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerVariable(name)
    

  override def toString() =
    name

}

object BoxerVariable {

}
