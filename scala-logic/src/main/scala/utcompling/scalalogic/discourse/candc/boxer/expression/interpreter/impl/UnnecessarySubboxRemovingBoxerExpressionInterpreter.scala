package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable

class UnnecessarySubboxRemovingBoxerExpressionInterpreter extends BoxerExpressionInterpreter[BoxerExpression] {

  private var PropVar = """^(p\d*)$""".r

  override def interpret(e: BoxerExpression) = {
    val crushed = crush(e)._1
    if(crushed.isDefined)
      crushed.get
    else BoxerDrs(List(), List());
  }
  /**
   * @return a pair: (crushed expression, free variables found below)
   */
  private def crush(e: BoxerExpression): (Option[BoxerExpression], Set[BoxerVariable], Set[BoxerVariable]) =
  {
    e match {
      case BoxerAlfa(variable, first, second) =>
        val thisVars = Set[BoxerVariable]()
        val (firstCrushed, firstVars, firstPropVars) = crush(first)
        val (secondCrushed, secondVars, secondPropVars) = crush(second)
        //(BoxerAlfa(variable, firstCrushed, secondCrushed), thisVars | firstVars | secondVars)
        //Merge the two boxes of Alfa in one DRS BOX
        (merge(firstCrushed, secondCrushed), firstVars | secondVars, firstPropVars | secondPropVars)

       
      case BoxerApp(function, argument) =>
        val (functionCrushed, functionVars, functionPropVars) = crush(function)
        val (argumentCrushed, argumentVars, argumentPropVars) = crush(argument)
        if(functionCrushed.isDefined && argumentCrushed.isDefined)
        	(Some(BoxerApp(functionCrushed.get, argumentCrushed.get)), functionVars | argumentVars, functionPropVars | argumentPropVars)
        else if(functionCrushed.isDefined)
        	(functionCrushed, functionVars | argumentVars, functionPropVars | argumentPropVars)
        else if(argumentCrushed.isDefined)
        	(argumentCrushed, functionVars | argumentVars, functionPropVars | argumentPropVars)
        else
            (None, functionVars | argumentVars, functionPropVars | argumentPropVars)

      case BoxerDrs(refs, conds) =>
        val refVars = refs.map(_._2).toSet
        val crushes = conds.map(this.crush(_))
        val (resultConds, resultVars, resultPropVars) = crushes.unzip3;
        //val unprunableVars = resultVarsType.filterNot(_._2 == "p").map(_._1).flatten
        //val allResultVars = resultVars.fold(Set())(_ | _)
        val (additionalRefs, crushedConds) =
          resultConds.map {
            case Some(BoxerDrs(r, c)) =>
              (r.map(_._2), c)
            case None =>
              (List[BoxerVariable](), List())
            case e =>
              (List[BoxerVariable](), List(e.get))
          }.unzip
        val prunablePropVarsFlat = resultPropVars.flatten.toSet -- resultVars.flatten.toSet
        val filteredRefs = (refVars.toSet ++ additionalRefs.flatten.toSet) --  prunablePropVarsFlat
        //val filteredRefs = refs.filterNot(r => prunedPropVarsFlat(r._2))
        val flattenConds = crushedConds.flatten
        if(!flattenConds.isEmpty)
        	(Some(BoxerDrs(filteredRefs.map(r => (List[BoxerIndex](), r) ).toList, flattenConds)), resultVars.flatten.toSet, resultPropVars.flatten.toSet)
        else (None, resultVars.flatten.toSet, resultPropVars.flatten.toSet)

      case BoxerEq(discId, indices, first, second) =>
        (Some(BoxerEq(discId, indices, first, second)), Set(first, second), Set())

      case BoxerImp(discId, indices, first, second) =>
        val (firstCrushed, firstVars, firstPropVars) = crush(first)
        val (secondCrushed, secondVars, secondPropVars) = crush(second)
        if(firstCrushed.isDefined && secondCrushed.isDefined)
        	(Some(BoxerImp(discId, indices, firstCrushed.get, secondCrushed.get)), firstVars | secondVars, firstPropVars | secondPropVars)
	    else if(firstCrushed.isDefined)
	    	//(Some(BoxerNot(discId, indices, firstCrushed.get)), firstVars | secondVars)
	    	(Some(BoxerImp(discId, indices, firstCrushed.get, firstCrushed.get)), firstVars | secondVars, firstPropVars | secondPropVars)
	    else if(secondCrushed.isDefined)
	    	(secondCrushed, firstVars | secondVars, firstPropVars | secondPropVars)
	    else
	    	(None, firstVars | secondVars, firstPropVars | secondPropVars)

      case BoxerOr(discId, indices, first, second) =>
        val (firstCrushed, firstVars, firstPropVars) = crush(first)
        val (secondCrushed, secondVars, secondPropVars) = crush(second)
        if(firstCrushed.isDefined && secondCrushed.isDefined)
        	(Some(BoxerOr(discId, indices, firstCrushed.get, secondCrushed.get)), firstVars | secondVars, firstPropVars | secondPropVars)
	    else if(firstCrushed.isDefined)
	    	//(Some(BoxerNot(discId, indices, firstCrushed.get)), firstVars | secondVars)
	    	(firstCrushed, firstVars | secondVars, firstPropVars | secondPropVars)
	    else if(secondCrushed.isDefined)
	    	(secondCrushed, firstVars | secondVars, firstPropVars | secondPropVars)
	    else
	    	(None, firstVars | secondVars, firstPropVars | secondPropVars)

      case BoxerMerge(pred, first, second) =>
        val (firstCrushed, firstVars, firstPropVars) = crush(first)
        val (secondCrushed, secondVars, secondPropVars) = crush(second)
        //(BoxerMerge(pred, firstCrushed, secondCrushed), firstVars | secondVars)
        //Merge the two boxes of Merge in one DRS BOX        
        //(BoxerDrs(firstCrushed.refs ++ secondCrushed.refs, firstCrushed.conds ++ secondCrushed.conds), firstVars | secondVars)
        (merge(firstCrushed, secondCrushed), firstVars | secondVars, firstPropVars | secondPropVars)

      case BoxerNamed(discId, indices, variable, name, typ, sense) =>
        (Some(BoxerNamed(discId, indices, variable, name, typ, sense)), Set(variable), Set())

      case BoxerNot(discId, indices, drs) =>
        val (drsCrushed, drsVars, drsPropVars) = crush(drs)
        if(drsCrushed.isDefined)
        	(Some(BoxerNot(discId, indices, drsCrushed.get)), drsVars, drsPropVars)
        else
        	(None, drsVars, drsPropVars)

      case BoxerPred(discId, indices, variable, name, pos, sense) =>
        (Some(BoxerPred(discId, indices, variable, name, pos, sense)), Set(variable), Set())

      case BoxerProp(discId, indices, variable, drs) =>{
        val (drsCrushed, drsVars, drsPropVars) = crush(drs)//This has to be changed if we want to support
        														//embedded propositions
        if(drsCrushed.isDefined)
        	//(Some(BoxerProp(discId, indices, variable, drsCrushed.get)), drsVars, drsPropVars|Set(variable))
          (Some(drsCrushed.get), drsVars, drsPropVars|Set(variable))
        else
        	(None, drsVars, drsPropVars|Set(variable))
      }

      case BoxerRel(discId, indices, event, variable, name, sense) =>
        if(name == "theme")
        	(None, Set(), Set()) //Cuong suggested removing the predicate "theme". This should change if we want to support embedded propositions  
        else        
        	(Some(BoxerRel(discId, indices, event, variable, name, sense)), Set(variable, event), Set())
        
      case BoxerCard(discId, indices, variable, num, typ) =>
        (Some(BoxerCard(discId, indices, variable, num, typ)), Set(variable), Set())
      case _ => (Some(e), Set(), Set());
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
