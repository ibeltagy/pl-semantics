package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerDrs(override val refs: List[(List[BoxerIndex], BoxerVariable)], override val conds: List[BoxerExpression]) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(conds.map(function))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerDrs(refs.map (ref => (ref._1, function(ref._2).asInstanceOf[BoxerVariable])), conds.map(function))

  override def toString(): String = {
    val refs = this.refs.map { case (i, v) => "[" + i.mkString(",") + "]:" + v.name }
    return "drs([%s],[%s])".format(refs.mkString(","), conds.mkString(","))
  }

}

object BoxerDrs {
  def apply(conds: BoxerExpression*) = new BoxerDrs(List(), conds.toList)
  def apply(ref: BoxerVariable, conds: BoxerExpression*) = new BoxerDrs(List(List() -> ref), conds.toList)
  def apply(ref: (List[Int], BoxerVariable), conds: BoxerExpression*) = new BoxerDrs(List(ref._1.map(BoxerIndex(_)) -> ref._2), conds.toList)
  def apply(refs: List[(List[BoxerIndex], BoxerVariable)], conds: BoxerExpression*) = new BoxerDrs(refs, conds.toList)
}
