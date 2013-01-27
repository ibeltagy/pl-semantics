package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerOr(discId: String, indices: List[BoxerIndex], first: BoxerExpression, second: BoxerExpression) extends BoxerExpression {

    def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R = 
        combinator(List(function(first), function(second)))

    def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression = 
        BoxerOr(discId, indices, function(first), function(second))

    override def toString() =
        "[%s]:or(%s,%s)".format(indices.mkString(","), first, second)

}

object BoxerOr {

}