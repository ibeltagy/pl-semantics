package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.fol._

case class FolIfExpression(override val first: FolExpression, override val second: FolExpression)
    extends FolBooleanExpression(first, second) {

    override val operator = FolTokens.IMP

}

object FolIfExpression {
}
