package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.FolAtom
import utcompling.scalalogic.util.StringUtils

case class DrtPropositionExpression(val variable: Variable, val drs: DrtExpression)
    extends DrtExpression {

    override def visit[S](function: DrtExpression => S, combinator: List[S] => S): S =
        combinator(List(function(this.drs)))

    override def visitStructured[S](function: DrtExpression => S, combinator: List[Any] => S) =
        combinator(List(this.variable, function(this.drs)))

    override def replace(variable: Variable, expression: DrtExpression, replace_bound: Boolean = false, alpha_convert: Boolean = true): DrtExpression =
        if (this.variable == variable)
            DrtPropositionExpression(
                expression.asInstanceOf[DrtVariableExpression].variable,
                this.drs.replace(variable, expression, replace_bound, alpha_convert))
        else
            DrtPropositionExpression(
                this.variable,
                this.drs.replace(variable, expression, replace_bound, alpha_convert))

    override def eliminateEquality() =
        DrtPropositionExpression(this.variable, this.drs.eliminateEquality())

    override def getRefs(recursive: Boolean = false) =
        if (recursive)
            this.drs.getRefs(true)
        else
            Set[Variable]()

    override def fol() =
        this.drs.fol()

    override def _folModal(world: Variable) =
        FolAtom(Variable("R"), world, this.variable) & this.drs._folModal(this.variable)

    override def pretty() =
        StringUtils.sideBySide("\n" + this.variable.name + ":", drs.pretty)

    override def toString() =
        "%s:%s".format(this.variable.name, this.drs)

}

object DrtPropositionExpression {
}

