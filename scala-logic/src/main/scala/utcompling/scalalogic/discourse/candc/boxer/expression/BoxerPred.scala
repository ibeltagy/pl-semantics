package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerPred(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, name: String, pos: String, sense: Int) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    default

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerPred(discId, indices, function(variable).asInstanceOf[BoxerVariable], name, pos, sense)

  def ::(index: Int) = BoxerPred(discId, List(BoxerIndex(index)), variable, name, pos, sense)

  override def toString(): String =
    "[%s]:pred(%s,%s,%s,%d)".format(indices.mkString(","), variable.name, name, pos, sense)

}

object BoxerPred {

}