package utcompling.scalalogic.base.expression

import scala.collection.mutable.ListBuffer
import utcompling.scalalogic.top.expression.Variable

trait BaseVariableBinderExpression[T <: BaseExpression[T]] extends BaseExpression[T] {

    val operator: String
    val variable: Variable
    val term: T

    /**
     * Rename all occurrences of the variable introduced by this variable binder in the expression to @C{newvar}.
     */
    def alphaConvert(newvar: Variable): BaseVariableBinderExpression[T] =
        this.construct(List(newvar, this.term.replace(this.variable, this.makeVariableExpression(newvar), true))).asInstanceOf[BaseVariableBinderExpression[T]]

    override def visit[S](function: T => S, combinator: List[S] => S) =
        combinator(List(function(this.term)))

    override def visitStructured[S](function: T => S, combinator: List[Any] => S) =
        combinator(List(this.variable, function(this.term)))

    override def replace(variable: Variable, expression: T, replace_bound: Boolean = false, alphaConvert: Boolean = true): T = {
        // if the bound variable is the thing being replaced
        if (this.variable == variable) {
            if (replace_bound) {
                require(expression.isInstanceOf[BaseVariableExpression[T]], expression + " is not a AbstractVariableExpression")
                return this.construct(List(expression.asInstanceOf[BaseVariableExpression[T]].variable, this.term.replace(variable, expression, true, alphaConvert)))
            } else
                return this.asInstanceOf[T]
        } else {
            // if the bound variable appears in the expression, then it must
            // be alpha converted to avoid a conflict
            var self: BaseVariableBinderExpression[T] = this
            if (alphaConvert && expression.free().contains(this.variable))
                self = self.alphaConvert(Variable.unique(pattern = this.variable))

            // replace in the term
            val v: Variable = self.variable
            val t: T = self.term
            return this.construct(List(v, t.replace(variable, expression, replace_bound, alphaConvert)))
        }
    }

    override def free() =
        this.term.free() - this.variable

    def getAllSameScopeBoundVariables(): (List[Variable], T) = {
        val vars = new ListBuffer[Variable]
        def collect(cur: BaseExpression[T]): BaseExpression[T] =
            cur match {
                case e: BaseVariableBinderExpression[T] if (e.operator == this.operator) => {
                    vars += e.variable
                    collect(e.term)
                }
                case _ => cur
            }
        val baseterm = collect(this).asInstanceOf[T]
        return (vars.result, baseterm)
    }

}
