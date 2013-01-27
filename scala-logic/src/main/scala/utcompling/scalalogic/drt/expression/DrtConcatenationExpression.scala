package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.drt._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.scalalogic.util.StringUtils

case class DrtConcatenationExpression(override val first: DrtExpression, override val second: DrtExpression, val consequent: Option[DrtExpression] = None)
    extends DrtBinaryExpression(first, second) {

    override val operator = DrtTokens.DRS_CONC

    override def visit[S](function: DrtExpression => S, combinator: List[S] => S): S =
        combinator(List(function(this.first), function(this.second)) ++ this.consequent.map(function))

    override def visitStructured[S](function: DrtExpression => S, combinator: List[Any] => S): S =
        combinator(List(function(this.first), function(this.second), this.consequent.map(function)))

    /**
     * Replace all instances of variable v with expression E in this, where v is free in this.
     */
    override def replace(variable: Variable, expression: DrtExpression, replace_bound: Boolean = false, alpha_convert: Boolean = true): DrtExpression = {
        var first = this.first
        var second = this.second
        var consequent = this.consequent

        // If variable is bound
        if (this.getRefs().contains(variable)) {
            if (replace_bound) {
                first = first.replace(variable, expression, replace_bound, alpha_convert)
                second = second.replace(variable, expression, replace_bound, alpha_convert)
                consequent = consequent.map(_.replace(variable, expression, replace_bound, alpha_convert))
            }
        } else {
            if (alpha_convert) {
                // alpha convert every ref that is free in "expression"
                for (ref <- (this.getRefs(true) & expression.free())) {
                    val v = DrtVariableExpression(Variable.unique(ref))
                    first = first.replace(ref, v, true, alpha_convert)
                    second = second.replace(ref, v, true, alpha_convert)
                    consequent = consequent.map(_.replace(ref, v, true, alpha_convert))
                }
            }

            first = first.replace(variable, expression, replace_bound, alpha_convert)
            second = second.replace(variable, expression, replace_bound, alpha_convert)
            consequent = consequent.map(_.replace(variable, expression, replace_bound, alpha_convert))
        }

        return DrtConcatenationExpression(first, second, consequent)
    }

    override def eliminateEquality(): DrtExpression = {
        //TODO: at some point.  for now, simplify.
        val drs = this.simplify()
        require(!drs.isInstanceOf[DrtConcatenationExpression])
        return drs.eliminateEquality()
    }

    override def simplify(): DrtExpression = {
        val first = this.first.simplify
        var second = this.second.simplify
        val consequent = this.consequent.map(_.simplify)

        if (first.isInstanceOf[DrtBoxExpression] && second.isInstanceOf[DrtBoxExpression]) {
            // For any ref that is in both "first" and "second"
            for (ref <- (first.getRefs(true) & second.getRefs(true))) {
                // alpha convert the ref in "second" to prevent collision
                val newvar = DrtVariableExpression(Variable.unique(ref))
                second = second.replace(ref, newvar, true, true)
            }
            return DrtBoxExpression(first.asInstanceOf[DrtBoxExpression].refs ++ second.asInstanceOf[DrtBoxExpression].refs,
                first.asInstanceOf[DrtBoxExpression].conds ++ second.asInstanceOf[DrtBoxExpression].conds,
                consequent)
        } else
            return DrtConcatenationExpression(first, second, consequent)
    }

    override def getRefs(recursive: Boolean = false): Set[Variable] = {
        val refs = this.first.getRefs(recursive) ++ this.second.getRefs(recursive)
        if (this.consequent.isDefined && recursive)
            return refs ++ this.consequent.get.getRefs(true)
        else
            return refs
    }

    override def fol(): FolExpression = {
        val e = this.first.fol() & this.second.fol()
        return this.consequent.map(c => e -> c.fol()).getOrElse(e)
    }

    override def _folModal(world: Variable): FolExpression = {
        val e = this.first._folModal(world) & this.second._folModal(world)
        return this.consequent.map(c => e -> c._folModal(world)).getOrElse(e)
    }

    override def pretty(): String = {
        val parts = this.getAllConnectedSameLevel().flatMap(e => List(" " + this.operator + " ", e.pretty)).tail
        val antecedent = StringUtils.sideBySideCentering((DrtTokens.OPEN :: parts ::: DrtTokens.CLOSE :: Nil): _*)
        return this.consequent.map(c => StringUtils.sideBySideCentering(DrtTokens.OPEN, antecedent, " ", DrtTokens.IMP, " ", c.pretty, DrtTokens.CLOSE)).getOrElse(antecedent)
    }

    override def toString(): String = {
        val antecedent = DrtTokens.OPEN + this.getAllConnectedSameLevel().mkString(" " + this.operator + " ") + DrtTokens.CLOSE
        return this.consequent.map(DrtTokens.OPEN + antecedent + " " + DrtTokens.IMP + " " + _ + DrtTokens.CLOSE).getOrElse(antecedent)
    }

}

object DrtConcatenationExpression {
}
