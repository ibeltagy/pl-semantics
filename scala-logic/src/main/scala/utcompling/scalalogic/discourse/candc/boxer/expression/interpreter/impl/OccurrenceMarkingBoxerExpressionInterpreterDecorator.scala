package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter

class OccurrenceMarkingBoxerExpressionInterpreterDecorator extends BoxerExpressionInterpreter[BoxerExpression] {

  override def interpret(e: BoxerExpression) =
    e match {
      case BoxerNamed(discId, indices, variable, name, typ, sense) =>
        //BoxerNamed(discId, indices, variable, "%s_%s%s".format(name, typ, indexString(discId, indices)), typ, sense)
        BoxerNamed(discId, indices, variable, "%s_%s".format(name, typ), typ, sense)
      case BoxerPred(discId, indices, variable, name, pos, sense) =>
        //BoxerPred(discId, indices, variable, "%s_%s%s".format(name, pos, indexString(discId, indices)), pos, sense)
        BoxerPred(discId, indices, variable, "%s_%s_d%s".format(name, pos, discId), pos, sense)
      case BoxerRel(discId, indices, event, variable, name, sense) =>
        //BoxerRel(discId, indices, event, variable, "r_%s%s".format(name, indexString(discId, indices)), sense)
        //BoxerRel(discId, indices, event, variable, "r_%s".format(name), sense)
        BoxerRel(discId, indices, event, variable, "r_%s_d%s".format(name, discId), sense)
      case _ =>
        e.visitConstruct(this.interpret)
    }

  protected def indexString(discId: String, indices: List[BoxerIndex]) = {
    if (indices.nonEmpty) {
      "_d%s%s".format(discId, indices.map("_" + _).mkString(""))
    }
    else
      ""
  }

}
