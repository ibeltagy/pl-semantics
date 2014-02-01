package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import utcompling.mlnsemantics.run.Sts
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import utcompling.mlnsemantics.inference.support.PriorExpression

object SetPriorPTP{
  var predPrior: Double = 0.0;
  var entPrior: Double = 0.0;
  var withText: Boolean = true;
}
    
class SetPriorPTP(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Seq[Double] = {
		  
    def predicateToPrior(expr: FolExpression):FolExpression =  //something like: forall x0...xn !pred(x0, ...., xn) 
    {
    	var priorExp:FolExpression = FolNegatedExpression(expr);
      	expr.getVariables.foreach(v => priorExp = FolAllExpression(v, priorExp));
      	priorExp
    }
    
    var priorExpressions:List[WeightedExpression[FolExpression]] = List();
    
    SetPriorPTP.predPrior = Sts.opts.task match //prior of the negated predicate 
    {
        case "sts" => 3; //prior in case of STS
        case "rte" => 1; //prior in case of RTE
    }
    SetPriorPTP.entPrior = SetPriorPTP.predPrior //entailment prior equals the other predicates priors

    if (Sts.opts.softLogicTool == "ss")
    {
      //No priors when using SS
    }
    //Only prior on Entailment. It is a function of another Infernece Step   
    else if (Sts.opts.fixDCA == true && Sts.opts.task == "rte" && Sts.opts.noHMinus == false /*ignoring H- for now until we fix inference*/) 
    {
      //Run inference for the first time to get the "default probability"
      SetPriorPTP.withText = false;
      val result = delegate.prove(constants, declarations, evidence, assumptions, goal);
      
      //if it works, generate the prior on entailment and run again.
      if (!result.isEmpty && result.head > 0)
      {
        val priorExp = predicateToPrior(SetVarBindPTP.entPred_h);
        val priorWeight = scala.math.log(result.head) - scala.math.log(1-result.head); //proof will be in the paper (isA) 
        priorExpressions = List(PriorExpression(priorExp.asInstanceOf[FolExpression], priorWeight));  
      }
      else return result;//if it does not work, return the result as it is. 
      
    }
    else  //Prior on all predicates
    {
      //for all declarations, generate prior expressions
      priorExpressions = declarations.map {
      	case (expr, varTypes) => {
      	  
      	  val priorExp = predicateToPrior(expr);
      	  
      	  //Different prior for the entailment predicate
      	  val priorWeight = (
      	      if (expr == SetVarBindPTP.entPred_h || expr == SetVarBindPTP.entPred_t) //if it is one of the entailment predicates  
      	        SetPriorPTP.entPrior //use entailment prior
      	      else
      	        SetPriorPTP.predPrior
      	  )
      	  PriorExpression(priorExp.asInstanceOf[FolExpression], priorWeight);
      	 }
      	}.toList
    }//end if/else.
    
    SetPriorPTP.withText = true;
    delegate.prove(constants, declarations, evidence, priorExpressions ++ assumptions, goal);
  }
}
