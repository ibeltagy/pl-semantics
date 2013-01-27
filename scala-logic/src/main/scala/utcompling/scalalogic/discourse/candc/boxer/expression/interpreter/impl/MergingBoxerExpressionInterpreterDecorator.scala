package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter

class MergingBoxerExpressionInterpreterDecorator extends BoxerExpressionInterpreter[BoxerExpression] {

  override def interpret(e: BoxerExpression) =
    e match {
      case BoxerAlfa(_variable, first, second) =>
        BoxerDrs(first.refs ++ second.refs, first.conds ++ second.conds)
      case BoxerMerge(_pred, first, second) =>
        BoxerDrs(first.refs ++ second.refs, first.conds ++ second.conds)
      case _ =>
        e.visitConstruct(this.interpret)
    }

}
