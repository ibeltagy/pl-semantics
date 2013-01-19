package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.top.expression.Expression
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.scalalogic.base.expression.BaseExpression
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.drt.DrtTokens

/**
 * This is the base abstract DRT Expression from which every DRT Expression extends.
 */
abstract case class DrtExpression
    extends Expression
    with BaseExpression[DrtExpression] {

    def unary_- = DrtNegatedExpression(this)
    def +(other: DrtExpression) = DrtConcatenationExpression(this, other)
    def |(other: DrtExpression) = DrtOrExpression(this, other)
    def ->(other: DrtExpression) = this match {
        case DrtApplicationExpression(function, argument, _) => DrtApplicationExpression(function, argument, Some(other))
        case DrtBoxExpression(refs, conds, _) => DrtBoxExpression(refs, conds, Some(other))
        case DrtConcatenationExpression(first, second, _) => DrtConcatenationExpression(first, second, Some(other))
    }

    override def applyto(other: DrtExpression) =
        DrtApplicationExpression(this, other)

    /**
     * Return the set of discourse referents in this DRS.
     * @param recursive: Also find discourse referents in subterms?
     */
    def getRefs(recursive: Boolean = false): Set[Variable]

    /**
     * Is self of the form "PRO(x)"?
     */
    def isPronounFunction() =
        this match {
            case e: DrtApplicationExpression => {
                e.function match {
                    case f: DrtVariableExpression => {
                        e.argument match {
                            case a: DrtVariableExpression => {
                                f.variable.name == DrtTokens.PRONOUN && Variable.isIndVar(a.variable.name)
                            }
                            case _ => false
                        }
                    }
                    case _ => false
                }
            }
            case _ => false
        }

    def makeEqualityExpression(first: DrtExpression, second: DrtExpression) =
        DrtEqualityExpression(first, second)

    override def makeVariableExpression(variable: Variable) =
        DrtVariableExpression(variable)

    /**
     * Draw the DRS
     */
    def pprint() =
        println(this.pretty())

    /**
     * Draw the DRS
     */
    def pretty(): String

    def eliminateEquality(): DrtExpression =
        this.visitStructured(e => e.eliminateEquality(), this.construct)

    def fol(): FolExpression

    final def folModal(): FolExpression = {
        val simplified = this.simplify
        val allrefs = simplified.getRefs(true)
        val top = Variable("top")
        val newVar =
            if (allrefs.contains(top))
                Variable.unique(top, allrefs)
            else
                top
        return simplified._folModal(newVar)
    }

    def _folModal(world: Variable): FolExpression

}
