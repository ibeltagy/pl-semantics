package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import utcompling.mlnsemantics.run.Sts
import scala.collection.mutable.MutableList
import utcompling.mlnsemantics.inference.support.GoalExpression
//import scala.actors.Futures._
//import scala.actors.threadpool.TimeoutException
import utcompling.mlnsemantics.inference.support.GoalExpression


class UniqueGoalEntitiesProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {
  	    
  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Seq[Double] = {
    

//    val newAssumptions:List[WeightedExpression[FolExpression]] = if(! (Sts.opts.uniqueEntities && Sts.opts.task == "sts" && Sts.opts.softLogicTool == "psl") ) assumptions
	val newAssumptions:List[WeightedExpression[FolExpression]] = if(true ) assumptions
    else
      assumptions.map 
      {
          case GoalExpression(e, w) => 
          {
        	  var vars = e.getVariables();
        	  vars = vars.filterNot(_ == Variable("")) 
        	  val eq = getEqualities(e);
        	  var extraExp:FolExpression = null;
        	  vars.foreach(v1=>{
        	    vars.foreach(v2=>{
        	    	if(v1 != v2 && !eq.contains((v1, v2)) && !eq.contains((v2, v1)) )
        	    	{
        	    	  if (extraExp == null)
        	    	    extraExp = FolNegatedExpression(FolEqualityExpression(FolVariableExpression(v1), FolVariableExpression(v2)));
        	    	  else
        	    	    extraExp = FolAndExpression(extraExp, FolNegatedExpression(FolEqualityExpression(FolVariableExpression(v1), FolVariableExpression(v2))))
        	    	}
        	    })
        	  })
        	  def insertExtraExp(e:FolExpression):FolExpression = 
        	  {
        	    e match {
        	      case FolAllExpression(v, term) => FolAllExpression(v, insertExtraExp(term)) 
        	      case FolIfExpression(first, second) => FolIfExpression(insertExtraExp(first), second); 
        	      case _ => FolAndExpression(e, extraExp);
        	    }
        	  }
        	  var exp = e;
        	  if(extraExp != null)
        		  exp = insertExtraExp(exp);
        	  GoalExpression(exp, w)
          }
          case a @ _ => a
      }
    
    delegate.prove(
      constants,
      declarations,
      evidence,
      newAssumptions,
      goal)
  }
        
  private def getEqualities(e: FolExpression): Seq[(Variable, Variable)] =
    e match {
      case FolEqualityExpression(first, second) => Seq((first.asInstanceOf[FolVariableExpression].variable, 
    		  										second.asInstanceOf[FolVariableExpression].variable));
      case FolVariableExpression(v) => Seq();
      case _ => e.visit(getEqualities, (parts: List[Seq[(Variable, Variable)]]) => parts.flatten)
    }
}
