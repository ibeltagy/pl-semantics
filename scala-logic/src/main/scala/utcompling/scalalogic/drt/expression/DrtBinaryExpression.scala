package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.base.expression.BaseBinaryExpression
import utcompling.scalalogic.drt.DrtTokens
import utcompling.scalalogic.util.StringUtils

abstract class DrtBinaryExpression(val first: DrtExpression, val second: DrtExpression)
    extends DrtExpression
    with BaseBinaryExpression[DrtExpression] {

    val operator: String

    override def getRefs(recursive: Boolean = false) =
        if (recursive)
            this.first.getRefs(true) ++ this.second.getRefs(true)
        else
            Set()

    override def pretty() =
        StringUtils.sideBySideCentering(DrtTokens.OPEN, first.pretty, " ", this.operator, " ", second.pretty, DrtTokens.CLOSE)

}
