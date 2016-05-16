package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.fol._

abstract class FolBooleanExpression(override val first: FolExpression, override val second: FolExpression)
    extends FolBinaryExpression(first, second) {

    override def _pretty(): List[String] = {
        val first = this.first._pretty
        val second = this.second._pretty
        return List(FolTokens.OPEN) ++ first.dropRight(1).map("  "+_) ++ List("  " + first.last + " " + operator) ++ 
                                       second.map("  "+_) ++ List(FolTokens.CLOSE)
    }

}
