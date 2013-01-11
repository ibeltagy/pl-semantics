package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression

class ExistentialEliminatingProbabilisticTheoremProver(
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
    goal: FolExpression): Option[Double] = {

    val b = Buffer[(String, String)]()

    def go(e: FolExpression): FolExpression = {
      e match {
        case FolExistsExpression(v, term) =>
          val newV = v.name.toUpperCase
          val typ = v.name.charAt(1) match { //char at index 0 is either t or h
            case 'x' => "indv"
            case 'e' => "evnt"
            case 'p' => "prop"
            case _ => "indv"
          }

          b += typ -> newV

          go(term.replace(v, FolVariableExpression(Variable(newV))))

        case _ =>
          e.visitStructured(go, e.construct)
      }
    }

    val newAssumptions: List[WeightedExpression[FolExpression]] =
      assumptions.map {
        case HardWeightedExpression(e) => HardWeightedExpression(go(e))
        case SoftWeightedExpression(e, w) => SoftWeightedExpression(go(e), w)
      }

    def goKeepOuter(e: FolExpression): FolExpression = {
	    e match {
	        case FolExistsExpression(v, term) => FolExistsExpression(v, goKeepOuter(term)) 
	        case _ => go (e) 
	    }
    }
    val newGoal = goKeepOuter(goal) 
    
    delegate.prove(
      constants +++ (b.toSet.groupByKey: Map[String, Set[String]]),
      declarations,
      evidence,
      newAssumptions,
      newGoal)

  }

}
