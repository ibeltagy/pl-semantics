package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import support.HardWeightedExpression
import utcompling.mlnsemantics.run.Sts
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable

class FromEntToEqvProbabilisticTheoremProver(
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
    goal: BoxerExpression): Option[Double] = {

	var prefix = "t";
    def renameVars(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerVariable(name) =>
            BoxerVariable(prefix + name)
        case _ => e.visitConstruct(renameVars)
      }
    }
    val prem = renameVars(assumptions.head.expression);
    prefix = "h";
    val hyp = renameVars(goal);
    
    

    val (newAssumptions, newGoal) = 
    Sts.opts.task match {
      case "rte" => {
        (List (HardWeightedExpression (prem)) ++ 
            assumptions.filterNot( _ == assumptions.head), 
	     hyp)
      }
      case "sts" => {
	    (List (HardWeightedExpression (hyp)) ++ 
	    	List (HardWeightedExpression (prem)) ++ 
	    	assumptions.filterNot( _ == assumptions.head), //add premise and hypothesis to assumptions 
	    BoxerDrs(List(), List(prem, hyp)) /*anding Goals*/)
      }
    }
    delegate.prove(constants, declarations, evidence, newAssumptions, newGoal)
  }
}
