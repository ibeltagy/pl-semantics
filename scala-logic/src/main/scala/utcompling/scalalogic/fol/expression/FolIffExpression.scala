package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.fol._

case class FolIffExpression(override val first: FolExpression, override val second: FolExpression)
    extends FolBooleanExpression(first, second) {

    override val operator = FolTokens.IFF

}

object FolIffExpression {
}
