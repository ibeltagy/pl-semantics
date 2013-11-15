package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import support.HardWeightedExpression
import utcompling.mlnsemantics.run.Sts
import utcompling.scalalogic.discourse.candc.boxer.expression._

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
    goal: BoxerExpression): Seq[Double] = {

	var prefix = "t";
    def renameVar(e: BoxerVariable): BoxerVariable = {
      return BoxerVariable(prefix + e.name);
    }
	def renameVars(e: BoxerExpression): BoxerExpression = {
      e match {
	      case BoxerEq(discId, indices, first, second) => BoxerEq(prefix, indices, renameVar(first), renameVar(second))
	      case BoxerImp(discId, indices, first, second) => BoxerImp(prefix, indices, first.visitConstruct(renameVars), second.visitConstruct(renameVars))
	      case BoxerNamed(discId, indices, variable, name, typ, sense) => BoxerNamed(prefix, indices, renameVar(variable), name, typ, sense)
	      case BoxerNot(discId, indices, drs) => BoxerNot(prefix, indices, drs.visitConstruct(renameVars))
	      case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(prefix, indices, renameVar(variable), name, pos, sense)
	      case BoxerProp(discId, indices, variable, drs) => BoxerProp(prefix, indices, renameVar(variable), drs.visitConstruct(renameVars))
	      case BoxerRel(discId, indices, event, variable, name, sense) => BoxerRel(prefix, indices, renameVar(event), renameVar(variable), name, sense)
	      case BoxerCard(discId, indices, variable, num, typ) => BoxerCard(prefix, indices, renameVar(variable), num, typ)
	      case BoxerTimex(discId, indices, variable, timeExp) => BoxerTimex(prefix, indices, renameVar(variable), timeExp)
	      case BoxerVariable(name) =>BoxerVariable(prefix + name)
	      case _ => e.visitConstruct(renameVars)
      }
    }

    val prem = assumptions.head.expression;
    val hyp = goal;

    val (newAssumptions, newGoal) = 
    Sts.opts.task match {
      case "rte" => {
        prefix = "h";
        val premH = renameVars(prem)
        val hypH = renameVars(hyp)
        (List (HardWeightedExpression (premH)) ++ 
            assumptions.filterNot( _ == assumptions.head), 
	     hypH)
      }
      case "sts" => {
        prefix = "h";
        val premH = renameVars(prem)
        val hypH = renameVars(hyp)
        prefix = "t";
        val premT = renameVars(hyp)
        val hypT = renameVars(prem)        
	    (List (HardWeightedExpression (premH)) ++ 
	    	List (HardWeightedExpression (premT)) ++ 
	    	assumptions.filterNot( _ == assumptions.head), //add premise and hypothesis to assumptions 
	    BoxerDrs(List(), List(hypH, hypT)) /*anding Goals*/)
      }
    }
    delegate.prove(constants, declarations, evidence, newAssumptions, newGoal)
  }
}
