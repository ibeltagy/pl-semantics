package utcompling.scalalogic.base.expression

import utcompling.scalalogic.base.Tokens
import utcompling.scalalogic.top.expression.Variable

trait BaseApplicationExpression[T <: BaseExpression[T]] extends BaseExpression[T] {

  val function: T
  val argument: T

  /**
   * Uncurry this application expression
   * @return: A tuple (base-function, arg-list)
   */
  def uncurry(): (T, List[T]) =
    function match {
      case ae: BaseApplicationExpression[T] => {
        val (f, a) = ae.uncurry
        (f, a :+ argument)
      }
      case _ =>
        (function, List(argument))
    }

  /**
   * Return uncurried base-function.
   * If this is an atom, then the result will be a variable expression.
   * Otherwise, it will be a lambda expression.
   */
  def pred(): T =
    this.uncurry()._1

  /**
   * Return uncurried arg-list
   */
  def args(): List[T] =
    this.uncurry()._2

  /**
   * Is this expression an atom (as opposed to a lambda expression applied
   * to a term)?
   */
  override def isAtom() =
    this.pred.isInstanceOf[BaseVariableExpression[T]]

  override def constants(): Set[Variable] = {
    val function_constants = this.function match {
      case v: BaseVariableExpression[T] => Set()
      case _ => this.function.constants()
    }
    return function_constants ++ this.argument.constants()
  }

  override def predicates(): Set[Variable] = {
    val function_preds = this.function match {
      case v: BaseVariableExpression[T] => {
        if (!Variable.isFuncVar(v.variable.name) &&
          !Variable.isIndVar(v.variable.name) &&
          !Variable.isEventVar(v.variable.name))
          Set(v.variable) //it's a constant
        else
          Set()
      }
      case _ => this.function.predicates()
    }
    return function_preds ++ this.argument.predicates()
  }

  override def toString(): String = {
    // uncurry the arguments and find the base function
    var (function, arg_str) =
      if (this.isAtom())
        (this.pred, this.args.mkString(", "))
      else
        (this.function, this.argument.toString) //Leave arguments curried

    var function_str =
      if (shouldParenthesizeFunction(function))
        Tokens.OPEN + function + Tokens.CLOSE
      else
        function.toString

    return function_str + Tokens.OPEN + arg_str + Tokens.CLOSE
  }

  protected def shouldParenthesizeFunction(function: T): Boolean = {
    if (function.isInstanceOf[BaseLambdaExpression[T]]) {
      val term = function.asInstanceOf[BaseLambdaExpression[T]].term
      if (term.isInstanceOf[BaseApplicationExpression[T]]) {
        if (!term.asInstanceOf[BaseApplicationExpression[T]].function.isInstanceOf[BaseVariableExpression[T]]) {
          return true
        }
      } else if (!function.asInstanceOf[BaseLambdaExpression[T]].term.isInstanceOf[BaseBinaryExpression[T]]) {
        return true
      }
    } else if (function.isInstanceOf[BaseApplicationExpression[T]]) {
      return true
    }
    return false
  }

}