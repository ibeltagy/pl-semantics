package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.fol._

case class FolOrExpression(override val first: FolExpression, override val second: FolExpression)
    extends FolBooleanExpression(first, second) {

    override val operator = FolTokens.OR

    override def toString(): String =
        FolTokens.OPEN + this.getAllConnectedSameLevel().mkString(" " + operator + " ") + FolTokens.CLOSE

    override def _pretty(): List[String] = {
        val parts = this.getAllConnectedSameLevel().map(_._pretty)
        return List(FolTokens.OPEN) ++ 
        parts.dropRight(1).flatMap(part => part.dropRight(1).map("  "+_) ++ List("  " + part.last + " " + operator)) ++ 
        parts.last.map("  "+_) ++ 
        List(FolTokens.CLOSE)
    }

}

object FolOrExpression {
}
