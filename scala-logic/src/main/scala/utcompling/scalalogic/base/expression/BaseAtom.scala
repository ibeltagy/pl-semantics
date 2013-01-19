package utcompling.scalalogic.base.expression

import utcompling.scalalogic.top.expression.Variable

trait BaseAtom[A <: BaseApplicationExpression[T], T <: BaseExpression[T]] {

    def apply(pred: Variable, args: Variable*) =
        (pred +: args).map(makeVariableExpression(_)).reduceLeft[T](_.applyto(_)).asInstanceOf[A]

    protected def makeVariableExpression(v: Variable): T

    def unapplySeq(ae: A): Option[(Variable, Seq[Variable])] = {
        if (ae.isAtom) {
            val (pred, args) = ae.uncurry
            if (pred.isInstanceOf[BaseVariableExpression[T]] && args.isInstanceOf[List[BaseVariableExpression[T]]]) {
                return Some(
                    pred.asInstanceOf[BaseVariableExpression[T]].variable,
                    args.map(_.asInstanceOf[BaseVariableExpression[T]].variable))
            }
        }
        return None
    }

}