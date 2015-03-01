package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import support.HardWeightedExpression
import utcompling.mlnsemantics.run.Sts
import utcompling.scalalogic.discourse.candc.boxer.expression._

object GivenNotTextProbabilisticTheoremProver{
  var negativeEvd: Boolean = true;
}


class GivenNotTextProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  /**
   * Return the proof, or None if the proof failed
   */
	def prove(
			constants: Map[String, Set[String]], // type -> constant
			declarations: Map[BoxerExpression, Seq[String]], // predicate -> seq[type] 
			evidence: List[BoxerExpression],
			assumptions: List[WeightedExpression[BoxerExpression]],
			goal: BoxerExpression): Seq[Double] = 
	{
		if (Sts.opts.task == "rte" && Sts.opts.withNegT && Sts.opts.softLogicTool != "psl")
		{
			/*if (Sts.opts.ratio)
			{
				//Sts.opts.withFixCWA = false;
				Sts.opts.wFixCWA = 0.5
						//Sts.opts.prior = 1
			}*/
			GivenNotTextProbabilisticTheoremProver.negativeEvd = true;
			var hGivenT = delegate.prove(constants, declarations, evidence, assumptions, goal);
			assert(hGivenT.length == 1)

			GivenNotTextProbabilisticTheoremProver.negativeEvd = true;
			var notHGivenT = //if (Sts.opts.ratio)
					delegate.prove(constants, declarations, evidence, assumptions, negateWithoutPresupposed(goal));
			//else
			//	Seq(0.0)
			assert(notHGivenT.length == 1)
			
			
         val newAssumptions = assumptions.map{
         case HardWeightedExpression(exp, w) => HardWeightedExpression (exp, 0.5)
         case a @ _ => a
         }

         var hGivenT2 = Seq(0.0)
			if (Sts.opts.ratio)
         	hGivenT2 = delegate.prove(constants, declarations, evidence, newAssumptions, goal);
         assert(hGivenT2.length == 1)

         var notHGivenT2 = Seq(0.0)
         if (Sts.opts.ratio)
            notHGivenT2 = delegate.prove(constants, declarations, evidence, newAssumptions, negateWithoutPresupposed(goal));
         assert(notHGivenT2.length == 1)

/*
			GivenNotTextProbabilisticTheoremProver.negativeEvd = true;

			var hGivenNothing = (
					if (Sts.opts.ratio)
						delegate.prove(constants, declarations, evidence, newAssumptions, goal);
					else Seq(0.0)
					)
					assert(hGivenNothing.length == 1)

					GivenNotTextProbabilisticTheoremProver.negativeEvd = false;
			var hGivenNothing2 = (
					if (Sts.opts.ratio)
						delegate.prove(constants, declarations, evidence, newAssumptions, goal);
					else Seq(0.0)
					)
					assert(hGivenNothing2.length == 1)

					var newGoal:BoxerExpression = null;
					var newText:HardWeightedExpression[BoxerExpression] = null
							val newAssumptions2 = assumptions.map{
							case HardWeightedExpression(exp, w) => {
								//newGoal = exp;
								//HardWeightedExpression (negateText(goal)) 
								//newGoal = negateText(exp);
								if(Sts.opts. ratio)
								{
									newGoal = /*negateWithoutPresupposed*/(exp)
									newText = HardWeightedExpression (/*moveAllIntoNegation*/(goal), w)
								}
								else
								{
									newGoal = negateWithoutPresupposed(exp)
									newText = HardWeightedExpression (goal, w)
								}
								var swp = Sts.textLemma;
								Sts.textLemma = Sts.hypothesisLemma
								Sts.hypothesisLemma = swp;
								swp = Sts.text;
								Sts.text = Sts.hypothesis
								Sts.hypothesis = swp;

								newText
							}
							//    	  case SoftWeightedExpression(BoxerImp(discId, List(), lhs, rhs), w) => SoftWeightedExpression(
							//    	  								BoxerImp(discId, List(), BoxerDrs(lhs.refs, rhs.conds), BoxerDrs(List(), lhs.conds)).asInstanceOf[BoxerExpression], w) 
							//          case a @ _ => a
					}
					//Wrong
					//val tGivenNotH = delegate.prove(constants, declarations, evidence, newAssumptions2, newGoal);
					//assert(tGivenNotH.length == 1)
					var notTGivenH = Seq(0.0)
					var notTGivenNotH = (
							if (Sts.opts.ratio)
							{
								var resNum = hGivenT
								var resDenum = hGivenNothing

								assert(resNum.length == 1)
								assert(resDenum.length == 1)

								var errorCode:Double = 0.0;
								if (resNum.head < 0)
									errorCode = errorCode + 1
									if (resDenum.head < 0)
										errorCode = errorCode + 1

										hGivenT = (if (resNum.head == resDenum.head && resNum.head == 0)
											Seq(1.0)
											else if (resDenum.head == 0)
												Seq(100.0)
												else if (resNum.head == 0)
													Seq(-1/resDenum.head)
													else
														Seq(resNum.head/resDenum.head)
												)
												//hGivenNothing = Seq(0.0)

												resNum = hGivenT2 //delegate.prove(constants, declarations, evidence, newAssumptions2, newGoal)
												val newAssumptions3  = newAssumptions2.filterNot (_ == newText) :+ HardWeightedExpression(newText.expression, 0.5);
												resDenum = hGivenNothing2 //delegate.prove(constants, declarations, evidence, newAssumptions3, newGoal)
														assert(resNum.length == 1)
												assert(resDenum.length == 1)

												if (resNum.head < 0)
													errorCode = errorCode + 1
													if (resDenum.head < 0)
														errorCode = errorCode + 1

														hGivenNothing = (if (resNum.head == resDenum.head && resNum.head == 0)
															Seq(1.0)
															else if (resDenum.head == 0)
																Seq(100.0)
																else if (resNum.head == 0)
																	Seq(-1/resDenum.head)
																	else
																		Seq(resNum.head/resDenum.head)
																)

																/*				if (hGivenT.head < 1)
				hGivenT
			else if (hGivenNothing.head < 1)
				hGivenNothing
			else
				hGivenT
																 */
																Seq(errorCode)
							}
							else
							{
								notTGivenH = delegate.prove(constants, declarations, evidence, newAssumptions2, newGoal)
								val newAssumptions3  = newAssumptions2.filterNot (_ == newText) :+ HardWeightedExpression(negateWithoutPresupposed(newText.expression), newText.weight);
								delegate.prove(constants, declarations, evidence, newAssumptions3, newGoal)
							}

							)
							assert(notTGivenH.length == 1)

							//hGivenT ++ hGivenNothing ++ notTGivenH
							hGivenT ++ notTGivenNotH ++ notHGivenT ++ notTGivenH
							 * 
							 */
			hGivenT ++ hGivenT2  ++ notHGivenT ++ notHGivenT2
		}
		else
			delegate.prove(constants, declarations, evidence, assumptions, goal)   
	}
  
  private def negateWithoutPresupposed(e:BoxerExpression) : BoxerExpression = 
  {
	e match {
		case BoxerMerge("smerge", first, second) => 
			BoxerOr("h", List(), negateWithoutPresupposed(first), negateWithoutPresupposed(second)) 
		case BoxerMerge("merge", first, second) => 
			BoxerDrs (List(), List(first,  BoxerNot("h", List(), second)))
		case e => BoxerNot("h", List(), e)
	}
  }
  private def moveAllIntoNegation (exp:BoxerExpression): BoxerExpression = 
  {
    exp match {
      case BoxerDrs(refs, conds) => {
    	  var negation:BoxerNot = null
    	  var foundOther:Boolean = false;
    	  conds.foreach(c=>{
    	   c match {
		      case BoxerPred(discId, indices, variable, name, pos, sense) => 
		      case BoxerNamed(discId, indices, variable, name, typ, sense) => 
		      case BoxerCard(discId, indices, variable, num, typ) => 
   		      case BoxerRel(discId, indices, event, variable, name, sense) => 
   		      case BoxerEq(discId, indices, first, second) =>
   		      case BoxerNot(discId, indices, drs) =>  negation = c.asInstanceOf[BoxerNot];
		      case _ => foundOther = true;
    	   } 
    	  });
    	  if (negation != null && !foundOther)
    	  {
    	  	val filteredConds = conds.filter(!_.isInstanceOf[BoxerNot]);
    	  	return BoxerNot (negation.discId, negation.indices, BoxerDrs(refs ++ negation.drs.refs, filteredConds ++ negation.drs.conds))
    	  }
    	  else return exp
      }
      case _ =>  return exp
    }
  }
  
  private var negationFound = false;
  private def negateText (e:BoxerExpression) : BoxerExpression = 
  {
	negationFound = false;
	var exp = findAndRemoveNegation (e);
	if (negationFound)
	  return exp;
	else 
	{
		return (exp match {
	    	case BoxerAlfa(variable, first, second) =>  
	    			BoxerDrs (List(), List(first,  negateRHS(second))).asInstanceOf[BoxerExpression]
	        case BoxerMerge(pred, first, second) => 
	    			BoxerDrs (List(), List(first,  negateRHS(second))).asInstanceOf[BoxerExpression]          
	        case e => negateRHS(e)
	    })
	}
  }
  
  var varsToMove:List[BoxerVariable] = null;
  var lhs:List[BoxerExpression] = null;
  var rhs:BoxerExpression = null;
  private def negateRHS (exp:BoxerExpression) : BoxerExpression =  //Find all subjects of AGENT, and negate the rest
  {
    varsToMove = List();
    findAgentVar(exp); //initializes varsToMove 
	var varsToMoveSize = 0;
	rhs = exp;
	lhs = List();
	while (varsToMoveSize != varsToMove.size)
	{
		varsToMoveSize = varsToMove.size;
		rhs = moveFromRhsToLhs (rhs);
	}
	BoxerDrs (varsToMove.map(v =>  (List(), v)), lhs :+ BoxerNot("h", List(), rhs)).asInstanceOf[BoxerExpression]
  }
  
  private def findAgentVar (exp:BoxerExpression):Any = 
  {
     exp match {
      //case BoxerAlfa(variable, first, second) => BoxerAlfa(variable, findAndRemoveNegation(first), findAndRemoveNegation(second)) 
      //case BoxerMerge(pred, first, second) => BoxerMerge(pred, findAndRemoveNegation(first), findAndRemoveNegation(second))       
      //case BoxerApp(function, argument) => BoxerApp(findAndRemoveNegation(function), findAndRemoveNegation(argument))
      //case BoxerProp(discId, indices, variable, drs) => BoxerProp(discId, indices, variable, findAndRemoveNegation(drs))      
      //case BoxerDrs(refs, conds) => BoxerDrs(refs, conds.map(findAndRemoveNegation(_)))
      case BoxerNot(discId, indices, drs) => None
      case BoxerImp(discId, indices, first, second) => None
      case BoxerOr(discId, indices, first, second) => None
      //case BoxerEq(discId, indices, first, second) => None
      //case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discId, indices, variable, name, pos, sense)
      //case BoxerNamed(discId, indices, variable, name, typ, sense) => BoxerNamed(discId, indices, variable, name, typ, sense)
      case BoxerRel(discId, indices, event, variable, "agent", sense) => varsToMove = varsToMove :+ variable
      //case BoxerCard(discId, indices, variable, num, typ) => BoxerCard(discId, indices, variable, num, typ)
      case _ => exp.visit(findAgentVar, (parts: List[Any]) => parts.head, None)
    }
    None
  }
  
  private def moveFromRhsToLhs (exp:BoxerExpression): BoxerExpression = 
  {
    exp match {
      case BoxerAlfa(variable, first, second) => BoxerAlfa(variable, moveFromRhsToLhs(first), moveFromRhsToLhs(second)) 
      case BoxerMerge(pred, first, second) => BoxerMerge(pred, moveFromRhsToLhs(first), moveFromRhsToLhs(second))       
      case BoxerApp(function, argument) => BoxerApp(moveFromRhsToLhs(function), moveFromRhsToLhs(argument))
      case BoxerProp(discId, indices, variable, drs) => BoxerProp(discId, indices, variable, moveFromRhsToLhs(drs))      
      case BoxerDrs(refs, conds) => {
    	  val filteredRefs = refs.filterNot(r=>varsToMove.contains(r._2));
    	  val filteredConds = conds.flatMap(c=>{
    	   c match {
		      case BoxerPred(discId, indices, variable, name, pos, sense) => moveFromRhsToLhs (c, variable)
		      case BoxerNamed(discId, indices, variable, name, typ, sense) => moveFromRhsToLhs (c, variable)
		      case BoxerCard(discId, indices, variable, num, typ) => moveFromRhsToLhs (c, variable)
		      case BoxerRel(discId, indices, event, variable, "agent", sense) => Some(c)
   		      case BoxerRel(discId, indices, event, variable, name, sense) => moveFromRhsToLhs(c, event, variable);
   		      case BoxerEq(discId, indices, first, second) => moveFromRhsToLhs(c, first.asInstanceOf[BoxerVariable], second.asInstanceOf[BoxerVariable]);
		      case _ => Some(moveFromRhsToLhs(c));
    	   } 
    	  });
    	  BoxerDrs(filteredRefs, filteredConds)
      }
      case BoxerNot(discId, indices, drs) =>  exp // This is only reachable in case of negated equalities
      case BoxerImp(discId, indices, first, second) => exp
      case BoxerOr(discId, indices, first, second) => exp
      case _ =>  throw new RuntimeException("not reachable");
    }
  }
  private def moveFromRhsToLhs (exp:BoxerExpression, v:BoxerVariable): Option[BoxerExpression] =
  {
    if(varsToMove.contains(v)) 
    {
        lhs = lhs:+ exp;   //it should be moved to the LHS
        None
    }else 
    	Some(exp) //Keep it in the RHS
  }
  
  private def moveFromRhsToLhs (exp:BoxerExpression, v1:BoxerVariable, v2:BoxerVariable): Option[BoxerExpression] =
  {
    val result1 = moveFromRhsToLhs(exp, v1)
    if(result1.isDefined)  //kept in RHS
    {
    	val result2 = moveFromRhsToLhs(exp, v2)
    	if(result2.isEmpty)  //Moved to LHS
    	  varsToMove = varsToMove :+ v1;
   		return result2
    }
    else
    {
    	varsToMove = varsToMove :+ v2;
    	return result1
    }
  }
  
  private def findAndRemoveNegation (exp:BoxerExpression) : BoxerExpression =
  {
    exp match {
      case BoxerAlfa(variable, first, second) => BoxerAlfa(variable, findAndRemoveNegation(first), findAndRemoveNegation(second)) 
      case BoxerMerge(pred, first, second) => BoxerMerge(pred, findAndRemoveNegation(first), findAndRemoveNegation(second))       
      case BoxerApp(function, argument) => BoxerApp(findAndRemoveNegation(function), findAndRemoveNegation(argument))
      case BoxerProp(discId, indices, variable, drs) => BoxerProp(discId, indices, variable, findAndRemoveNegation(drs))      
      case BoxerDrs(refs, conds) => BoxerDrs(refs, conds.map(findAndRemoveNegation(_)))

      case BoxerNot(discId, indices, drs) =>
      {
    	  drs match 
    	  {
    	    case BoxerDrs(ref, List(BoxerEq(discId, indices, first, second))) => BoxerNot(discId, indices, drs)
    	    case _ => {
    	      negationFound = true;
    	      drs
    	    }
    	  }
      }      
      case BoxerImp(discId, indices, first, second) => BoxerImp(discId, indices, first, second)
      case BoxerOr(discId, indices, first, second) => BoxerOr(discId, indices, first, second)
      case BoxerEq(discId, indices, first, second) => BoxerEq(discId, indices, first, second)
      case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discId, indices, variable, name, pos, sense)
      case BoxerNamed(discId, indices, variable, name, typ, sense) => BoxerNamed(discId, indices, variable, name, typ, sense)
      case BoxerRel(discId, indices, event, variable, name, sense) => BoxerRel(discId, indices, event, variable, name, sense)
      case BoxerCard(discId, indices, variable, num, typ) => BoxerCard(discId, indices, variable, num, typ)
      case _ =>  throw new RuntimeException("not reachable");
    }
  }
}

