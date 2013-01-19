package utcompling.scalalogic.base.expression

import utcompling.scalalogic.top.expression.Variable

trait BaseExpression[T <: BaseExpression[T]] {

    def apply(args: T*): T =
        return args.foldLeft(this.asInstanceOf[T])(_.applyto(_))

    def applyto(other: T): T

    def construct(parts: List[Any]): T = {
        val constructors = this.getClass.getConstructors()
        require(constructors.length == 1, "Argument runtime types must select exactly one constructor.  %d constructors found for %s".format(constructors.length, this.getClass))
        try {
            return constructors.head.newInstance(parts.asInstanceOf[List[java.lang.Object]]: _*).asInstanceOf[T]
        } catch {
            case e => throw new RuntimeException("Failure calling constructor for %s with arguments %s".format(this.getClass, parts), e)
        }
    }

    def visit[S](function: T => S, combinator: List[S] => S): S

    def visitStructured[S](function: T => S, combinator: List[Any] => S): S =
        this.visit(function, combinator)

    /**
     * Replace every instance of 'variable' with 'expression'
     * @param variable: C{Variable} The variable to replace
     * @param expression: C{Expression} The expression with which to replace it
     * @param replace_bound: C{boolean} Should bound variables be replaced?
     */
    def replace(variable: Variable, expression: T, replace_bound: Boolean = false, alpha_convert: Boolean = true): T = {
        return this.visitStructured(_.replace(variable, expression, replace_bound, alpha_convert), this.construct)
    }

    /**
     * Return a set of all the free (non-bound) variables.  This includes
     * both individual and predicate variables, but not constants.
     */
    def free(): Set[Variable] = {
        return this.visit(_.free, (parts: List[Set[Variable]]) => parts.flatten.toSet)
    }

    /**
     * Return a set of individual constants (non-predicates).
     */
    def constants(): Set[Variable] = {
        return this.visit(_.constants, (parts: List[Set[Variable]]) => parts.flatten.toSet)
    }

    /**
     * Return a set of predicates (constants, not variables).
     */
    def predicates(): Set[Variable] = {
        return this.visit(_.predicates, (parts: List[Set[Variable]]) => parts.flatten.toSet)
    }

    /**
     * Return beta-converted expression
     */
    def simplify(): T = {
        return this.visitStructured(_.simplify, this.construct)
    }

    /**
     * Rename auto-generated unique variables
     */
    def normalize(): T = {
        def get_indiv_vars(e: BaseExpression[T]): Set[Variable] = {
            e match {
                case v: BaseVariableExpression[T] => {
                    if (Variable.isIndVar(v.variable.name))
                        return Set(v.variable)
                    else
                        return Set()
                }
                case _ => {
                    return e.visit(get_indiv_vars, (parts: List[Set[Variable]]) => parts.flatten.toSet)
                }
            }
        }

        var result = this
        for ((v, i) <- get_indiv_vars(this).toList.sorted.zipWithIndex) {
            val newVar = {
                if (Variable.isEventVar(v.name))
                    Variable("e0%s".format(i + 1))
                else if (Variable.isIndVar(v.name))
                    Variable("z%s".format(i + 1))
                else
                    v
            }
            result = result.replace(v, this.makeVariableExpression(newVar), true)
        }
        return result.asInstanceOf[T]
    }
    
    /**
     * Is this expression an atom?
     */
    def isAtom() =
        false


    def makeVariableExpression(variable: Variable): T

}
