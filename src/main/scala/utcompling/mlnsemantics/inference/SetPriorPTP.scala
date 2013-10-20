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
}
    
class SetPriorPTP(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Option[Double] = {

    //first run with variable binding   
    if (Sts.opts.fixDCA == true && Sts.opts.task == "rte") {
      delegate.prove(constants, declarations, evidence, assumptions, goal);
    }
    else 
    {
      SetPriorPTP.predPrior = Sts.opts.task match //prior of the negated predicate 
      {
        case "sts" => 3; //prior in case of STS
        case "rte" => 1; //prior in case of RTE
      }
      SetPriorPTP.entPrior = SetPriorPTP.predPrior
      val priorExpressions:List[WeightedExpression[FolExpression]] = declarations.map {
      	case (expr, varTypes) => {
      	  var priorExp:FolExpression = FolNegatedExpression(expr);
      	  val variables = expr.getVariables;
      	  
      	  variables.foreach(v => priorExp = FolAllExpression(v, priorExp));
      	  
      	  //Different prior for the entailment predicate
      	  val priorWeight = (
      	      if (expr == SetVarBindPTP.entPred_h || expr == SetVarBindPTP.entPred_t)
      	        SetPriorPTP.entPrior
      	      else
      	        SetPriorPTP.predPrior
      	  )
      	  PriorExpression(priorExp.asInstanceOf[FolExpression], priorWeight);
      	}
      }.toList
      delegate.prove(constants, declarations, evidence, priorExpressions ++ assumptions, goal);
    }
  }
}
