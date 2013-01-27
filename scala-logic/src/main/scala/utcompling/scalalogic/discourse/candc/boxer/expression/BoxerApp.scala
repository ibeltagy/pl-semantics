package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerApp(function: BoxerExpression, argument: BoxerExpression) extends BoxerExpression {

  def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R =
    combinator(List(function(this.function), function(this.argument)))

  def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression =
    BoxerApp(function(this.function), function(this.argument))

  override def toString() =
    "app(%s,%s)".format(function, argument)

}

object BoxerApp {
  def apply(function: BoxerExpression, argument: String): BoxerApp =
    BoxerApp(function, BoxerVariable(argument))

  def apply(function: String, argument: BoxerExpression): BoxerApp =
    BoxerApp(BoxerVariable(function), argument)

  def apply(function: String, argument: String): BoxerApp =
    BoxerApp(BoxerVariable(function), BoxerVariable(argument))

}