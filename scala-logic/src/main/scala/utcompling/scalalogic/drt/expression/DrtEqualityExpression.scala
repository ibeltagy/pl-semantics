package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.drt._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.FolEqualityExpression

case class DrtEqualityExpression(override val first: DrtExpression, override val second: DrtExpression)
    extends DrtBinaryExpression(first, second) {

    override val operator = DrtTokens.EQ

    override def fol() =
        FolEqualityExpression(this.first.fol(), this.second.fol())

    override def _folModal(world: Variable) =
        FolEqualityExpression(this.first._folModal(world), this.second._folModal(world))

}

object DrtEqualityExpression {
}
