package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerLam(variable: BoxerVariable, term: BoxerExpression) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(List(function(term)))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerLam(variable, function(term))

  override def toString() =
    "lam(%s,%s)".format(variable, term)

}

object BoxerLam {

  def apply(variable: String, term: BoxerExpression): BoxerLam =
    BoxerLam(BoxerVariable(variable), term)

  def apply(vars: List[Any], term: BoxerExpression): BoxerExpression =
    vars.foldRight[BoxerExpression](term)((v, t) =>
      BoxerLam(v match {
        case v: BoxerVariable => v
        case s: String => BoxerVariable(s)
      }, t))

}
