package utcompling.mlnsemantics.flat

import utcompling.scalalogic.discourse.candc.boxer.expression._
import scala.collection.immutable.List
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression._
import scala.collection.mutable.ListBuffer
import utcompling.scalalogic.util.Counter

class FlatPremiseBoxerExpressionInterpreter extends BoxerExpressionInterpreter[List[FolExpression]] {

  override def interpret(e: BoxerExpression): List[FolExpression] = {
    val labelCounter = new LabelCounter
    val topLabel = labelCounter.get
    return TrueAtom(topLabel) :: clausify(e, topLabel, labelCounter)
  }

  private def clausify(ex: BoxerExpression, label: String, labelCounter: LabelCounter): List[FolExpression] =
    ex match {
      case BoxerDrs(refs, conds) => {
        conds.flatMap(clausify(_, label, labelCounter))
      }
      case BoxerImp(_, _, first, second) => {
        val aLabel = labelCounter.get
        val cLabel = labelCounter.get
        val tempPropVars = new ListBuffer[String]
        val firstClauses: List[FolExpression] = clausify(first, label, labelCounter).map {
          case PredAtom(_, label, name, variable) => PredExistAtom(label, name, variable)
          case NamedAtom(_, label, name, variable) => NamedExistAtom(label, name, variable)
          case RelAtom(_, label, name, event, variable) => RelExistAtom(label, name, event, variable)
          case EqAtom(_, label, first, second) => EqExistAtom(label, first, second)
        }
        val clauses = new ListBuffer[FolExpression]

        clauses += ImpAtom(label, aLabel, cLabel)

        if (first.refs.nonEmpty) {
          //if all of the antecedent's parts are true in the enclosing scope, then the antecedent is true
          //true(label) -> all <vars>.((<antecedent conds>) -> true(aLabel))
          val vars = first.refs.map(v => Variable(v._2.name))
          clauses += TrueAtom(label) ->
            (((firstClauses.reduceLeft(_ & _) exists tempPropVars.result.map(Variable(_))) ->
              (TrueAtom(aLabel) & vars.map(v => FolEqualityExpression(FolVariableExpression(Variable(v.name)), FolVariableExpression(Variable('"' + v.name + '"'))))
                .reduceLeft[FolExpression](_ & _))) all vars)
        }
        else
          clauses ++= firstClauses

        clauses ++= clausify(second, cLabel, labelCounter)

        clauses.result
      }
      case BoxerEq(_, _, first, second) => {
        List(EqAtom(label, label, labelCounter.get(first), labelCounter.get(second)))
      }
      case BoxerNamed(_, _, variable, name, typ, sense) => {
        List(NamedAtom(label, label, name, labelCounter.get(variable)))
      }
      case BoxerNot(_, _, drs) => {
        val newLabel = labelCounter.get
        NotAtom(label, newLabel) :: clausify(drs, newLabel, labelCounter)
      }
      case BoxerPred(_, _, variable, name, pos, sense) => {
        List(PredAtom(label, label, name, labelCounter.get(variable)))
      }
      case BoxerProp(_, _, variable, drs) => {
        val newLabel = labelCounter.get(variable)
        PropAtom(label, newLabel) :: clausify(drs, newLabel, labelCounter)
      }
      case BoxerRel(_, _, event, variable, name, sense) => {
        List(RelAtom(label, label, name, labelCounter.get(event), labelCounter.get(variable)))
      }
    }

}
