package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.base.expression.BaseVariableExpression
import utcompling.scalalogic.fol.expression.FolVariableExpression

case class DrtVariableExpression(variable: Variable)
    extends DrtExpression
    with BaseVariableExpression[DrtExpression] {

    override def fol() =
        FolVariableExpression(this.variable)

    override def _folModal(world: Variable) =
        this.fol()

    override def getRefs(recursive: Boolean = false): Set[Variable] =
        Set[Variable]()

    override def pretty() =
        this.toString

    override def eliminateEquality(): DrtVariableExpression =
        this

}
