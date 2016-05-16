package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.base.expression.BaseVariableBinderExpression
import utcompling.scalalogic.fol._

abstract class FolVariableBinderExpression(override val operator: String, override val variable: Variable, override val term: FolExpression)
    extends FolExpression
    with BaseVariableBinderExpression[FolExpression] {

    override def toString(): String = {
        val (vars, baseterm) = this.getAllSameScopeBoundVariables()
        return operator + " " + vars.map(_.name).mkString(" ") + FolTokens.DOT + baseterm
    }

    override def _pretty(): List[String] = {
        val (vars, baseterm) = this.getAllSameScopeBoundVariables()
        return (operator + " " + vars.sortWith(_ < _).map(_.name).mkString(" ") + FolTokens.DOT) +: baseterm._pretty.map("  "+_)
    }

}
