package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerCard(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, num: String, typ: String) extends BoxerExpression {

    def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R = 
        default

    def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression = 
        BoxerCard(discId, indices, variable, num,typ)

    def ::(index: Int) = BoxerCard(discId, List(BoxerIndex(index)), variable, num,typ)

    override def toString(): String =
      "[%s]:card(%s,%s,%s)".format(indices.mkString(","), variable.name, num,typ)

}

object BoxerCard {

}