package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter

class UnnecessarySubboxRemovingBoxerExpressionInterpreter extends BoxerExpressionInterpreter[BoxerExpression] {

  private var PropVar = """^(p\d*)$""".r

  override def interpret(e: BoxerExpression) =
    e match {
      case BoxerDrs(refs, conds) =>
        val (crushedE, eVars) = crush(e, Set())
        crushedE
      case _ =>
        e.visitConstruct(this.interpret)
    }

  /**
   * @return a pair: (crushed expression, free variables found below)
   */
  private def crush(e: BoxerExpression, propVarsInScope: Set[BoxerVariable]): (BoxerExpression, Set[BoxerVariable]) =
    e match {
      case BoxerAlfa(variable, first, second) =>
        val thisVars = Set[BoxerVariable]()
        val (firstCrushed, firstVars) = crush(first, propVarsInScope | thisVars)
        val (secondCrushed, secondVars) = crush(second, propVarsInScope | thisVars)
        (BoxerAlfa(variable, firstCrushed, secondCrushed), thisVars | firstVars | secondVars)

      case BoxerApp(function, argument) =>
        val (functionCrushed, functionVars) = crush(function, propVarsInScope)
        val (argumentCrushed, argumentVars) = crush(argument, propVarsInScope)
        (BoxerApp(functionCrushed, argumentCrushed), functionVars | argumentVars)

      case BoxerDrs(refs, conds) =>
        val refVars = refs.map(_._2).toSet
        val (resultConds, resultVars) = conds.map(e => this.crush(e, propVarsInScope | refVars)).unzip
        val allResultVars = resultVars.fold(Set())(_ | _)
        val (additionalRefs, crushedConds, prunedPropVars) =
          resultConds.map {
            case BoxerProp(discId, indices, variable, drs) if allResultVars(variable) && indices.isEmpty =>
              (drs.refs, drs.conds, Set[BoxerVariable](variable))
            case e =>
              (List[(List[BoxerIndex], BoxerVariable)](), List(e), Set[BoxerVariable]())
          }.unzip3
        val prunedPropVarsFlat = prunedPropVars.flatten.toSet
        val filteredRefs = refs.filterNot(r => prunedPropVarsFlat(r._2))
        (BoxerDrs(filteredRefs ++ additionalRefs.flatten, crushedConds.flatten), allResultVars -- refVars)

      case BoxerEq(discId, indices, first, second) =>
        (BoxerEq(discId, indices, first, second), Set(first, second))

      case BoxerImp(discId, indices, first, second) =>
        val (firstCrushed, firstVars) = crush(first, propVarsInScope)
        val (secondCrushed, secondVars) = crush(second, propVarsInScope)
        (BoxerImp(discId, indices, firstCrushed, secondCrushed), firstVars | secondVars)

      case BoxerMerge(pred, first, second) =>
        val (firstCrushed, firstVars) = crush(first, propVarsInScope)
        val (secondCrushed, secondVars) = crush(second, propVarsInScope)
        (BoxerMerge(pred, firstCrushed, secondCrushed), firstVars | secondVars)

      case BoxerNamed(discId, indices, variable, name, typ, sense) =>
        (BoxerNamed(discId, indices, variable, name, typ, sense), Set(variable))

      case BoxerNot(discId, indices, drs) =>
        val (drsCrushed, drsVars) = crush(drs, propVarsInScope)
        (BoxerNot(discId, indices, drsCrushed), drsVars)

      case BoxerPred(discId, indices, variable, name, pos, sense) =>
        (BoxerPred(discId, indices, variable, name, pos, sense), Set(variable))

      case BoxerProp(discId, indices, variable, drs) =>
        (BoxerProp(discId, indices, variable, drs), Set(variable))

      case BoxerRel(discId, indices, event, variable, name, sense) =>
        (BoxerRel(discId, indices, event, variable, name, sense), Set(variable))
        
      case BoxerCard(discId, indices, variable, num, typ) =>
        (BoxerCard(discId, indices, variable, num, typ), Set(variable))
      case _ => (e, Set());
    }
}
