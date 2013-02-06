package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerNamed(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, name: String, typ: String, sense: Int) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    default

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerNamed(discId, indices, variable, name, typ, sense)

  override def toString(): String =
    "[%s]:named(%s,%s,%s,%d)".format(indices.mkString(","), variable.name, nameToMlnIdentifier(name), typ, sense)

}

object BoxerNamed {

}