package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerTimex(discId: String, indices: List[BoxerIndex], variable:BoxerVariable, timeExp: BoxerExpression) extends BoxerExpression {

    def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R = 
        default

    def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression = 
        BoxerTimex(discId, indices, function(variable).asInstanceOf[BoxerVariable], function(timeExp))

    def ::(index: Int) = BoxerTimex(discId, List(BoxerIndex(index)), variable, timeExp)

    override def toString() =
        "[%s]:timex(%s,%s)".format(indices.mkString(","), variable, timeExp)

}

object BoxerTimeExp {

}