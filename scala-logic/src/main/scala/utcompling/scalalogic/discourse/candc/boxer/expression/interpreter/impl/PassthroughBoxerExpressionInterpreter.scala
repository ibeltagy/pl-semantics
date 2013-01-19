package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.top.expression.Variable

class PassthroughBoxerExpressionInterpreter extends BaseBoxerExpressionInterpreter[BoxerExpression] {

    override protected def interpretBoxerAlfa(variable: BoxerVariable, first: BoxerExpression, second: BoxerExpression) =
        BoxerAlfa(variable, interpret(first), interpret(second))

    override protected def interpretBoxerDrs(refs: List[(List[BoxerIndex], BoxerVariable)], conds: List[BoxerExpression]) =
        BoxerDrs(refs, conds.map(interpret))

    override protected def interpretBoxerEq(discId: String, indices: List[BoxerIndex], first: BoxerVariable, second: BoxerVariable) =
        BoxerEq(discId, indices, first, second)

    override protected def interpretBoxerImp(discId: String, indices: List[BoxerIndex], first: BoxerExpression, second: BoxerExpression) =
        BoxerImp(discId, indices, interpret(first), interpret(second))
    
    override protected def interpretBoxerEqv(discId: String, indices: List[BoxerIndex], first: BoxerExpression, second: BoxerExpression) =
        BoxerEqv(discId, indices, interpret(first), interpret(second))

    override protected def interpretBoxerMerge(pred: String, first: BoxerExpression, second: BoxerExpression) =
        BoxerMerge(pred, interpret(first), interpret(second))

    override protected def interpretBoxerNamed(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, name: String, typ: String, sense: Int) =
        BoxerNamed(discId, indices, variable, name, typ, sense)

    override protected def interpretBoxerNot(discId: String, indices: List[BoxerIndex], drs: BoxerExpression) =
        BoxerNot(discId, indices, interpret(drs))

    override protected def interpretBoxerPred(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, name: String, pos: String, sense: Int) =
        BoxerPred(discId, indices, variable, name, pos, sense)

    override protected def interpretBoxerProp(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, drs: BoxerExpression) =
        BoxerProp(discId, indices, variable, interpret(drs))

    override protected def interpretBoxerRel(discId: String, indices: List[BoxerIndex], event: BoxerVariable, variable: BoxerVariable, name: String, sense: Int) =
        BoxerRel(discId, indices, event, variable, name, sense)
        
    override protected def interpretBoxerCard(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, num: String, typ: String) =
        BoxerCard(discId, indices, variable, num, typ)
    
    override protected def interpretBoxerOr(discId: String, indices: List[BoxerIndex], first: BoxerExpression, second: BoxerExpression) =
        BoxerOr(discId, indices, interpret(first), interpret(second))
        
    override protected def interpretBoxerTimex(discId: String, indices: List[BoxerIndex], variable: BoxerVariable, timeExp: BoxerExpression) =
    	BoxerTimex(discId, indices, variable, interpret(timeExp))
    	
    override protected def interpretBoxerDate(indicesPol: List[BoxerIndex], pol: String, indicesYear: List[BoxerIndex], year: String, indicesMonth: List[BoxerIndex], month: String, indicesDay:List[BoxerIndex], day: String) =
    	BoxerDate(indicesPol, pol, indicesYear, year, indicesMonth, month, indicesDay, day)
}
