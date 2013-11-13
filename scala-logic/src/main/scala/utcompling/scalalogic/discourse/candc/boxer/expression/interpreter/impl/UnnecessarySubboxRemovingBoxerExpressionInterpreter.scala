package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable

class UnnecessarySubboxRemovingBoxerExpressionInterpreter extends BoxerExpressionInterpreter[BoxerExpression] {

  private var PropVar = """^(p\d*)$""".r

  override def interpret(e: BoxerExpression) = {
    val crushed = crush(e, Set())._1
    if(crushed.isDefined)
      crushed.get
    else BoxerDrs(List(), List())
  }
  /**
   * @return a pair: (crushed expression, free variables found below)
   */
  private def crush(e: BoxerExpression, propVarsInScope: Set[BoxerVariable]): (Option[BoxerExpression], Set[BoxerVariable]) =
  {
    e match {
      case BoxerAlfa(variable, first, second) =>
        val thisVars = Set[BoxerVariable]()
        val (firstCrushed, firstVars) = crush(first, propVarsInScope | thisVars)
        val (secondCrushed, secondVars) = crush(second, propVarsInScope | thisVars)
        //(BoxerAlfa(variable, firstCrushed, secondCrushed), thisVars | firstVars | secondVars)
        //Merge the two boxes of Alfa in one DRS BOX
        (merge(firstCrushed, secondCrushed), firstVars | secondVars)

       
      case BoxerApp(function, argument) =>
        val (functionCrushed, functionVars) = crush(function, propVarsInScope)
        val (argumentCrushed, argumentVars) = crush(argument, propVarsInScope)
        if(functionCrushed.isDefined && argumentCrushed.isDefined)
        	(Some(BoxerApp(functionCrushed.get, argumentCrushed.get)), functionVars | argumentVars)
        else if(functionCrushed.isDefined)
        	(functionCrushed, functionVars | argumentVars)
        else if(argumentCrushed.isDefined)
        	(argumentCrushed, functionVars | argumentVars)
        else
            (None, functionVars | argumentVars)

      case BoxerDrs(refs, conds) =>
        val refVars = refs.map(_._2).toSet
        //val (resultConds, resultVars) = conds.map(e => this.crush(e, propVarsInScope | refVars)).unzip
        val crushes = conds.map(e => e match {
          case BoxerProp(discId, indices, variable, drs) => (this.crush(e, propVarsInScope | refVars), "p")
          case _ => (this.crush(e, propVarsInScope | refVars), "o")
        })
        val (result, expType) = crushes.unzip;
        val (resultConds, resultVars) = result.unzip;
        val resultVarsType = resultVars zip expType 
        val unprunableVars = resultVarsType.filterNot(_._2 == "p").map(_._1).flatten
        val allResultVars = resultVars.fold(Set())(_ | _)
        val (additionalRefs, crushedConds, prunedPropVars) =
          resultConds.map {
            case Some(BoxerProp(discId, indices, variable, drs)) if allResultVars(variable) && indices.isEmpty =>
              (drs.refs, drs.conds, Set[BoxerVariable](variable))
            case Some(BoxerDrs(r, c)) =>
              (r, c, Set[BoxerVariable]())
            case None =>
              (List[(List[BoxerIndex], BoxerVariable)](), List(), Set[BoxerVariable]())
            case e =>
              (List[(List[BoxerIndex], BoxerVariable)](), List(e.get), Set[BoxerVariable]())
          }.unzip3
        val prunedPropVarsFlat = prunedPropVars.flatten.toSet -- unprunableVars.toSet
        val filteredRefs = refs.filterNot(r => prunedPropVarsFlat(r._2))
        val flattenConds = crushedConds.flatten
        if(!flattenConds.isEmpty)
        	(Some(BoxerDrs(filteredRefs ++ additionalRefs.flatten, flattenConds)), allResultVars -- refVars)
        else (None, allResultVars -- refVars)

      case BoxerEq(discId, indices, first, second) =>
        (Some(BoxerEq(discId, indices, first, second)), Set(first, second))

      case BoxerImp(discId, indices, first, second) =>
        val (firstCrushed, firstVars) = crush(first, propVarsInScope)
        val (secondCrushed, secondVars) = crush(second, propVarsInScope)
        if(firstCrushed.isDefined && secondCrushed.isDefined)
        	(Some(BoxerImp(discId, indices, firstCrushed.get, secondCrushed.get)), firstVars | secondVars)
	    else if(firstCrushed.isDefined)
	    	//(Some(BoxerNot(discId, indices, firstCrushed.get)), firstVars | secondVars)
	    	(Some(BoxerImp(discId, indices, firstCrushed.get, firstCrushed.get)), firstVars | secondVars)
	    else if(secondCrushed.isDefined)
	    	(secondCrushed, firstVars | secondVars)
	    else
	    	(None, firstVars | secondVars)

      case BoxerOr(discId, indices, first, second) =>
        val (firstCrushed, firstVars) = crush(first, propVarsInScope)
        val (secondCrushed, secondVars) = crush(second, propVarsInScope)
        if(firstCrushed.isDefined && secondCrushed.isDefined)
        	(Some(BoxerOr(discId, indices, firstCrushed.get, secondCrushed.get)), firstVars | secondVars)
	    else if(firstCrushed.isDefined)
	    	//(Some(BoxerNot(discId, indices, firstCrushed.get)), firstVars | secondVars)
	    	(firstCrushed, firstVars | secondVars)
	    else if(secondCrushed.isDefined)
	    	(secondCrushed, firstVars | secondVars)
	    else
	    	(None, firstVars | secondVars)

      case BoxerMerge(pred, first, second) =>
        val (firstCrushed, firstVars) = crush(first, propVarsInScope)
        val (secondCrushed, secondVars) = crush(second, propVarsInScope)
        //(BoxerMerge(pred, firstCrushed, secondCrushed), firstVars | secondVars)
        //Merge the two boxes of Merge in one DRS BOX        
        //(BoxerDrs(firstCrushed.refs ++ secondCrushed.refs, firstCrushed.conds ++ secondCrushed.conds), firstVars | secondVars)
        (merge(firstCrushed, secondCrushed), firstVars | secondVars)

      case BoxerNamed(discId, indices, variable, name, typ, sense) =>
        (Some(BoxerNamed(discId, indices, variable, name, typ, sense)), Set(variable))

      case BoxerNot(discId, indices, drs) =>
        val (drsCrushed, drsVars) = crush(drs, propVarsInScope)
        if(drsCrushed.isDefined)
        	(Some(BoxerNot(discId, indices, drsCrushed.get)), drsVars)
        else
        	(None, drsVars)

      case BoxerPred(discId, indices, variable, name, pos, sense) =>
        (Some(BoxerPred(discId, indices, variable, name, pos, sense)), Set(variable))

      case BoxerProp(discId, indices, variable, drs) =>{
        val (drsCrushed, drsVars) = crush(drs, propVarsInScope)//This has to be changed if we want to support
        														//embedded propositions
        if(drsCrushed.isDefined)
        	(Some(BoxerProp(discId, indices, variable, drsCrushed.get)), drsVars|Set(variable))
        else
        	(None, drsVars)
      }

      case BoxerRel(discId, indices, event, variable, name, sense) =>
        if(name == "theme")
        	(None, Set()) //Cuong suggested removing the predicate "theme". This should change if we want to support embedded propositions  
        else        
        	(Some(BoxerRel(discId, indices, event, variable, name, sense)), Set(variable))
        
      case BoxerCard(discId, indices, variable, num, typ) =>
        (Some(BoxerCard(discId, indices, variable, num, typ)), Set(variable))
      case _ => (Some(e), Set());
    }
  }
  
  private def merge(firstCrushed:Option[BoxerExpression], secondCrushed:Option[BoxerExpression]): Option[BoxerExpression] = 
  {
	if(firstCrushed.isDefined && secondCrushed.isDefined)
		Some(BoxerDrs(firstCrushed.get.refs ++ secondCrushed.get.refs, firstCrushed.get.conds ++ secondCrushed.get.conds))
    else if(firstCrushed.isDefined)
    	firstCrushed
    else if(secondCrushed.isDefined)
    	secondCrushed
    else
    	None
  }
}
