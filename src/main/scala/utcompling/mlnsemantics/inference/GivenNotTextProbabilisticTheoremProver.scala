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
          case HardWeightedExpression(BoxerAlfa(variable, first, second)) => first
        		  HardWeightedExpression (BoxerDrs (List(), List(first,  BoxerNot("h", List(), second))).asInstanceOf[BoxerExpression])
          case HardWeightedExpression(BoxerMerge(pred, first, second)) => first
        		  HardWeightedExpression (BoxerDrs (List(), List(first,  BoxerNot("h", List(), second))).asInstanceOf[BoxerExpression])          
          case HardWeightedExpression(e) =>
        		  HardWeightedExpression(BoxerNot("h", List(), e).asInstanceOf[BoxerExpression])
          case a @ _ => a
        }
    	val hGivenNotT = delegate.prove(constants, declarations, evidence, newAssumptions, goal);
    	assert(hGivenNotT.length == 1)
    	hGivenT ++ hGivenNotT 
    }
    else
      delegate.prove(constants, declarations, evidence, assumptions, goal)

    
  }
}
