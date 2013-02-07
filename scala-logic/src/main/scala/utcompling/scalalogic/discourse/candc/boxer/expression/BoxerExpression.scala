package utcompling.scalalogic.discourse.candc.boxer.expression

abstract class BoxerExpression {

  val forbiddenChars = """-|_|\.|,|\+|#|@|\$|%""".r;
  
  def nameToMlnIdentifier(name: String ): String = 
  {
		  var mlnId = forbiddenChars.replaceAllIn(name, "");
		  if (mlnId.length() == 0 )
		    mlnId = "id";
		  else if (!mlnId.charAt(0).isLetter)
			  mlnId = "id_" + mlnId;
		  return mlnId;
  }
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

}
