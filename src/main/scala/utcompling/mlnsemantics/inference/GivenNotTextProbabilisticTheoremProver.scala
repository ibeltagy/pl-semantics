package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import support.HardWeightedExpression
import utcompling.mlnsemantics.run.Sts
import utcompling.scalalogic.discourse.candc.boxer.expression._

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
    goal: BoxerExpression): Seq[Double] = {

    if (Sts.opts.task == "rte" && Sts.opts.withNegT && Sts.opts.softLogicTool != "psl")
    {
    	val hGivenT = delegate.prove(constants, declarations, evidence, assumptions, goal);
    	assert(hGivenT.length == 1)
    	val newAssumptions = assumptions.map{
    	    case HardWeightedExpression(exp) => HardWeightedExpression (negateText(exp))
          case a @ _ => a
        }
    	val hGivenNotT = delegate.prove(constants, declarations, evidence, newAssumptions, goal);
    	assert(hGivenNotT.length == 1)
    	
    	var newGoal:BoxerExpression = null;
    	val newAssumptions2 = assumptions.map{
    	    case HardWeightedExpression(exp) => {
    	    	//newGoal = exp;
    	    	//HardWeightedExpression (negateText(goal)) 
    	    	//newGoal = negateText(exp);
    	    	newGoal = BoxerNot("h", List(), exp).asInstanceOf[BoxerExpression]
    	    	HardWeightedExpression (goal)
    	    }
          case a @ _ => a
        }
    	
    	//Wrong
    	//val tGivenNotH = delegate.prove(constants, declarations, evidence, newAssumptions2, newGoal);
    	//assert(tGivenNotH.length == 1)
    	
    	//Hoefully correct
    	val notTGivenH = delegate.prove(constants, declarations, evidence, newAssumptions2, newGoal);
    	//val notTGivenH:Seq[Double] = Seq(0.0)
    	assert(notTGivenH.length == 1)
    	
    	hGivenT ++ hGivenNotT ++ notTGivenH
    }
    else
      delegate.prove(constants, declarations, evidence, assumptions, goal)   
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

