package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.drt._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.util.StringUtils

case class DrtOrExpression(override val first: DrtExpression, override val second: DrtExpression)
    extends DrtBooleanExpression(first, second) {

    override val operator = DrtTokens.OR

    override def fol() =
        this.first.fol() | this.second.fol()

    override def _folModal(world: Variable) =
        this.first._folModal(world) | this.second._folModal(world)

    override def pretty(): String = {
        val operator = "" +
            " \\  / " +
            "  \\/  "
        val parts = this.getAllConnectedSameLevel().flatMap(e => List(operator, e.pretty)).tail
        return StringUtils.sideBySideCentering(("\n" + DrtTokens.OPEN :: parts ::: "\n" :: DrtTokens.CLOSE :: Nil): _*)
    }

    override def toString() =
        DrtTokens.OPEN + this.getAllConnectedSameLevel().mkString(" " + this.operator + " ") + DrtTokens.CLOSE

}

object DrtOrExpression {
}
