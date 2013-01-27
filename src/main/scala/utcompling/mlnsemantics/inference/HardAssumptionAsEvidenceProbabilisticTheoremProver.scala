package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression

class HardAssumptionAsEvidenceProbabilisticTheoremProver(
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

    def go(e: FolExpression): List[FolExpression] = {
      e match {
        case FolAndExpression(first, second) => go(first) ++ go(second)
        case _ => List(e)
      }
    }

    val (evidenceAssumptions: List[WeightedExpression[FolExpression]], newAssumptions: List[WeightedExpression[FolExpression]]) =
      assumptions
        .flatMap {
          case HardWeightedExpression(e) => {
        	  println(e);
        	  go(e).map(HardWeightedExpression(_))
          }
          case a @ SoftWeightedExpression(e, w) => List(a)
        }
        .partition {
          case HardWeightedExpression(e @ FolAtom(_, _*)) => true
          case _ => false
        }

    delegate.prove(
      constants,
      declarations,
      evidence ++ evidenceAssumptions.map { case HardWeightedExpression(e) => e },
      newAssumptions,
      goal)

  }

}
