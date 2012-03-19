package utcompling.scalalogic.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression

trait ProbabilisticTheoremProver[T, R] {

  /**
   * Return the proof, or None if the proof failed
   */
  final def prove(goal: T): Option[R] =
    prove(List(), goal)

  /**
   * Return the proof, or None if the proof failed
   */
  def prove(assumptions: List[WeightedExpression[T]], goal: T): Option[R]

}
