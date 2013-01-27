package utcompling.scalalogic.inference

trait TheoremProver[T, R] {

    /**
     * Return the proof, or None if the proof failed
     */
    final def prove(goal: T): Option[R] =
        prove(List(), Some(goal), false)

    /**
     * Return the proof, or None if the proof failed
     */
    final def prove(goal: T, verbose: Boolean): Option[R] =
        prove(List(), None, verbose)

    /**
     * Return the proof, or None if the proof failed
     */
    final def prove(assumptions: List[T], goal: T): Option[R] =
        prove(assumptions, Some(goal), false)

    /**
     * Return the proof, or None if the proof failed
     */
    final def prove(assumptions: List[T], goal: T, verbose: Boolean): Option[R] =
        prove(assumptions, Some(goal), verbose)

    /**
     * Return the proof, or None if the proof failed
     */
    def prove(assumptions: List[T], goal: Option[T] = None, verbose: Boolean = false): Option[R]

}
