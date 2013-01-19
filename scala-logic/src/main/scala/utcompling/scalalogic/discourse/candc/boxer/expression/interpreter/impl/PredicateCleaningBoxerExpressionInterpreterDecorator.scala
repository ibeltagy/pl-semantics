package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter

class PredicateCleaningBoxerExpressionInterpreterDecorator extends BoxerExpressionInterpreter[BoxerExpression] {

  override def interpret(e: BoxerExpression) =
    e match {
      case BoxerNamed(discId, indices, variable, name, typ, sense) =>
        BoxerNamed(discId, indices, variable, name.replace(".", "_"), typ, sense)
      case BoxerPred(discId, indices, variable, name, pos, sense) =>
        BoxerPred(discId, indices, variable, name.replace(".", "_"), pos, sense)
      case BoxerRel(discId, indices, event, variable, name, sense) =>
        BoxerRel(discId, indices, event, variable, name.replace(".", "_"), sense)
      case _ =>
        e.visitConstruct(this.interpret)
    }

}
