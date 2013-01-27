package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.fol._

case class FolEqualityExpression(override val first: FolExpression, override val second: FolExpression)
    extends FolBinaryExpression(first, second) {

    override val operator = FolTokens.EQ

}

object FolEqualityExpression {
}
