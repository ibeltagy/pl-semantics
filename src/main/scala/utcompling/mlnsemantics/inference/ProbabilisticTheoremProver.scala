package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression.FolExpression

trait ProbabilisticTheoremProver[T] {

  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[String, Seq[String]], // predicate -> seq[type] 
    evidence: List[T],
    assumptions: List[WeightedExpression[T]],
    goal: T): Option[Double]

}
