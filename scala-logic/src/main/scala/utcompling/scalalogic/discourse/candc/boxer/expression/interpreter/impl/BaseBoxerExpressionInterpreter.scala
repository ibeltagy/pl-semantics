package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl

import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.base.expression.BaseExpression
import utcompling.scalalogic.discourse.candc.boxer.expression._

abstract class BaseBoxerExpressionInterpreter[T] extends BoxerExpressionInterpreter[T] {

    override def interpret(ex: BoxerExpression): T =
        ex match {
            case BoxerAlfa(variable, first, second) =>
                interpretBoxerAlfa(variable, first, second)
            case BoxerDrs(refs, conds) =>
                interpretBoxerDrs(refs, conds)
            case BoxerEq(discId, indices, first, second) =>
                interpretBoxerEq(discId, indices, first, second)
            case BoxerImp(discId, indices, first, second) =>
                interpretBoxerImp(discId, indices, first, second)
            case BoxerEqv(discId, indices, first, second) =>
                interpretBoxerEqv(discId, indices, first, second)
            case BoxerMerge(pred, first, second) =>
                interpretBoxerMerge(pred, first, second)
            case BoxerNamed(discId, indices, variable, name, typ, sense) =>
                interpretBoxerNamed(discId, indices, variable, name, typ, sense)
            case BoxerNot(discId, indices, drs) =>
                interpretBoxerNot(discId, indices, drs)
            case BoxerPred(discId, indices, variable, name, pos, sense) =>
                interpretBoxerPred(discId, indices, variable, name, pos, sense)
            case BoxerProp(discId, indices, variable, drs) =>
                interpretBoxerProp(discId, indices, variable, drs)
            case BoxerRel(discId, indices, event, variable, name, sense) =>
                interpretBoxerRel(discId, indices, event, variable, name, sense)
            case BoxerCard(discId, indices, variable, num, typ) =>
                interpretBoxerCard(discId, indices, variable, num, typ)
            case BoxerOr(discId, indices, first, second) =>
                interpretBoxerOr(discId, indices, first, second)
            case BoxerTimex(discId, indices, variable, timeExp) =>
                interpretBoxerTimex(discId, indices, variable, timeExp)
            case BoxerDate(indicesPol, pol, indicesYear, year, indicesMonth, month, indicesDay, day) =>
            	interpretBoxerDate(indicesPol, pol, indicesYear, year, indicesMonth, month, indicesDay, day)
        }

    protected def interpretBoxerAlfa(variable: BoxerVariable, first: BoxerExpression, second: BoxerExpression): T
    protected def interpretBoxerDrs(refs: List[(List[BoxerIndex], BoxerVariable)], conds: List[BoxerExpression]): T
    protected def interpretBoxerEq(discId: String, indices: List[BoxerIndex], first: BoxerVariable, second: BoxerVariable): T
    protected def interpretBoxerImp(discId: String, indices: List[BoxerIndex], first: BoxerExpression, second: BoxerExpression): T
    protected def interpretBoxerEqv(discId: String, indices: List[BoxerIndex], first: BoxerExpression, second: BoxerExpression): T
    protected def interpretBoxerMerge(pred: String, first: BoxerExpression, second: BoxerExpression): T
    protected def interpretBoxerNamed(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, name: String, typ: String, sense: Int): T
    protected def interpretBoxerNot(discId: String, indices: List[BoxerIndex], drs: BoxerExpression): T
    protected def interpretBoxerPred(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, name: String, pos: String, sense: Int): T
    protected def interpretBoxerProp(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, drs: BoxerExpression): T
    protected def interpretBoxerRel(discId: String, indices: List[BoxerIndex], event: BoxerVariable, variable: BoxerVariable, name: String, sense: Int): T
    protected def interpretBoxerCard(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, num: String, typ: String): T
    protected def interpretBoxerOr(discId: String, indices: List[BoxerIndex], first: BoxerExpression, second: BoxerExpression): T
    protected def interpretBoxerTimex(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, timeExp: BoxerExpression): T
    protected def interpretBoxerDate(indicesPol: List[BoxerIndex], pol: String, indicesYear: List[BoxerIndex], year: String, indicesMonth: List[BoxerIndex], month: String, indicesDay:List[BoxerIndex], day: String): T
}