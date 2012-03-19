package utcompling.mlnsemantics.flat

import utcompling.scalalogic.discourse.candc.boxer.expression._
import scala.collection.immutable.List
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.util.Counter
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MapBuilder

class FlatHypothesisBoxerExpressionInterpreter extends BoxerExpressionInterpreter[FolExpression] {

  override def interpret(e: BoxerExpression): FolExpression = {
    val structureLabels = new LabelCounter
    val topLabel = '"' + structureLabels.get + '"'
    val curLabels = new LabelCounter
    val clauses = clausify(e, topLabel, curLabels, true)
    val conjunct = clauses.reduceLeft(_ & _)
    return conjunct exists conjunct.free()
  }

  private def clausify(e: BoxerExpression, cur: String, curLabels: LabelCounter, isTrue: Boolean): List[FolExpression] =
    e match {
      case BoxerDrs(refs, conds) => {
        conds.flatMap(clausify(_, cur, curLabels, isTrue))
      }
      case BoxerEq(_, _, first, second) => {
        List(EqAtom(curLabels.get, cur, first.name, second.name))
      }
      case BoxerImp(_, _, first, second) => {
        null
      }
      case BoxerNot(_, _, drs) => {
        clausify(drs, cur, curLabels, !isTrue)
      }
      case BoxerProp(_, _, variable, drs) => {
        clausify(drs, cur, curLabels, isTrue) //TODO:
      }

      case _ => {
        val atom = e match {
          case BoxerNamed(_, _, variable, name, typ, sense) =>
            NamedAtom(curLabels.get, cur, name, variable.name)
          case BoxerPred(_, _, variable, name, pos, sense) =>
            PredAtom(curLabels.get, cur, name, variable.name)
          case BoxerRel(_, _, event, variable, name, sense) =>
            RelAtom(curLabels.get, cur, name, event.name, variable.name)
        }
        List(if (isTrue) atom else -atom)
      }
    }

}