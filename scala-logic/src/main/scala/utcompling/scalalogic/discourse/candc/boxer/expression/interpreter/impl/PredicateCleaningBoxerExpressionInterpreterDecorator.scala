package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter

class PredicateCleaningBoxerExpressionInterpreterDecorator extends BoxerExpressionInterpreter[BoxerExpression] {

  val specialChars = """([\?!\";\|\[\].,'_<>:\+\*-/&\^%\$#@~`=\(\)\\])""".r;

  def clearName(name: String): String =
    {
      var mlnId = specialChars.replaceAllIn(name, "_"); //remove all special characters. 
      mlnId = mlnId.map(c => {
        if (c.toShort > 127) 'X' //remove non-ascii characters 
        else c;
      })
      return mlnId;
    }

  override def interpret(e: BoxerExpression) =
    e match {
      case BoxerPred(discId, indices, variable, name, pos, sense) =>
        BoxerPred(discId, indices, variable, clearName(name), pos, sense)
      case BoxerRel(discId, indices, event, variable, name, sense) =>
        BoxerRel(discId, indices, event, variable, clearName(name), sense)
      case BoxerNamed(discId, indices, variable, name, typ, sense) =>
        BoxerNamed(discId, indices, variable, clearName(name), typ, sense)
      case _ =>
        e.visitConstruct(this.interpret)
    }

}