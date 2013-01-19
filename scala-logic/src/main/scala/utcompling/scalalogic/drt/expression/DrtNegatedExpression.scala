package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.base.expression.BaseNegatedExpression
import utcompling.scalalogic.drt.DrtTokens
import utcompling.scalalogic.util.StringUtils

case class DrtNegatedExpression(val term: DrtExpression)
    extends DrtExpression
    with BaseNegatedExpression[DrtExpression] {

    override def fol() =
        -this.term.fol()

    override def _folModal(world: Variable) =
        -this.term._folModal(world)

    override def getRefs(recursive: Boolean = false) =
        this.term.getRefs(recursive)

    override def pretty(): String = {
        val operator = "" +
            "__ \n" +
            "  |"
        return StringUtils.sideBySideCentering(operator, " ", term.pretty)
    }

    override def toString() =
        DrtTokens.NOT + term

}

object DrtNegatedExpression {
}
