package utcompling.scalalogic.base.expression

import utcompling.scalalogic.top.expression.Variable

trait BaseVariableExpression[T <: BaseExpression[T]] extends BaseExpression[T] {

    val variable: Variable

    override def visit[S](function: T => S, combinator: List[S] => S) =
        throw new NotDefinedError("VariableExpression.visit() is not defined")
    
    override def visitStructured[S](function: T => S, combinator: List[Any] => S) =
    	combinator(List(this.variable))

    override def replace(variable: Variable, expression: T, replace_bound: Boolean = false, alpha_convert: Boolean = true) =
        if (this.variable == variable)
            expression
        else
            this.asInstanceOf[T]

    override def free() =
        if (Variable.isIndVar(this.variable.name))
            Set(this.variable)
        else if (Variable.isFuncVar(this.variable.name))
            Set(this.variable)
        else if (Variable.isEventVar(this.variable.name))
            Set(this.variable)
        else
            Set()

    override def constants() =
        if (Variable.isIndVar(this.variable.name))
            Set()
        else if (Variable.isFuncVar(this.variable.name))
            Set()
        else if (Variable.isEventVar(this.variable.name))
            Set()
        else
            Set(this.variable)

    override def predicates() =
        Set()

    override def simplify() =
        this.asInstanceOf[T]

    override def toString() =
        variable.name
}
