package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerDate(indicesPol: List[BoxerIndex], pol: String, indicesYear: List[BoxerIndex], year: String, indicesMonth: List[BoxerIndex], month: String, indicesDay:List[BoxerIndex], day: String) extends BoxerExpression {

    def visit[R](function: BoxerExpression => R, combinator: List[R] => R, default: R): R = 
        default

    def visitConstruct(function: BoxerExpression => BoxerExpression): BoxerExpression = 
        BoxerDate(indicesPol, pol, indicesYear, year, indicesMonth, month, indicesDay, day)

    def ::(index: Int) = BoxerDate(indicesPol, pol, indicesYear, year, indicesMonth, month, indicesDay, day)

    override def toString(): String =
        "date([%s]:%s, [%s]:%s, [%s]:%s, [%s]:%s)".format(indicesPol.mkString(","), pol, indicesYear.mkString(","), year, indicesMonth.mkString(","), month, indicesDay.mkString(","), day)

}

object BoxerDate {

}
