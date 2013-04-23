package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerPrs(exps: List[(BoxerExpression, Double)]) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(exps.map(p=> function(p._1)))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerPrs(exps.map(p=>(function(p._1), p._2)))

  override def toString(): String = {
    return "prs(%s)".format(exps.flatMap(p=> List(p._1.toString(), p._2)).mkString(","))
  }
}

object BoxerPrs {
  def apply(exps: (BoxerExpression, Double)*) = new BoxerPrs(exps.toList)
}
