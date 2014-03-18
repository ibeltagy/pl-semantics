package utcompling.scalalogic.discourse.candc.boxer.expression

abstract class BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression

  def refs: List[(List[BoxerIndex], BoxerVariable)] =
    this match {
      case BoxerDrs(refs, conds) =>
        refs
      case BoxerMerge(_, first, second) =>
        first.refs ++ second.refs
      case BoxerAlfa(_, first, second) =>
        first.refs ++ second.refs
    }

  def conds: List[BoxerExpression] =
    this match {
      case BoxerDrs(refs, conds) =>
        conds
      case BoxerMerge(_, first, second) =>
        first.conds ++ second.conds
      case BoxerAlfa(_, first, second) =>
        first.conds ++ second.conds
    }

  def apply(args: BoxerExpression*) =
    args.foldLeft(this)((f, a) => BoxerApp(f, a))

  def unary_- = BoxerNot("", List(), this)
  def +(other: BoxerExpression) = BoxerMerge("merge", this, other)

  def getPredicates() : Seq[BoxerPred] = getPredicates(this);
  private def getPredicates(e: BoxerExpression): Seq[BoxerPred] =
    e match {
      case p: BoxerPred => Seq(p)
      case BoxerNamed(discId, indices, variable, name, typ, sense) => Seq(BoxerPred(discId, indices, variable, name, "n", sense))
      case _ => e.visit(getPredicates, (parts: List[Seq[BoxerPred]]) => parts.flatten, Seq.empty[BoxerPred])
    }

  def getRelations() : Seq[BoxerRel] = getRelations(this);
  private def getRelations(e: BoxerExpression): Seq[BoxerRel] =
    e match {
      case p: BoxerRel => Seq(p)
      case _ => e.visit(getRelations, (parts: List[Seq[BoxerRel]]) => parts.flatten, Seq.empty[BoxerRel])
    }

  def getEqualities() : Seq[BoxerEq] = getEqualities(this);
  private def getEqualities(e: BoxerExpression): Seq[BoxerEq] =
    e match {
      case p: BoxerEq => Seq(p)
      case _ => e.visit(getEqualities, (parts: List[Seq[BoxerEq]]) => parts.flatten, Seq.empty[BoxerEq])
    }

}
