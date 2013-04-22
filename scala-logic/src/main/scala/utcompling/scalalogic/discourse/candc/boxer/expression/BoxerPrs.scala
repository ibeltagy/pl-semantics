package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerPrs(exps: List[BoxerExpression]) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(exps.map(function))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerPrs(exps.map(function))

  override def toString(): String = {
    return "prs(%s)".format(exps.mkString(","))
  }
}

object BoxerPrs {
  def apply(exps: BoxerExpression*) = new BoxerPrs(exps.toList)
}
