package utcompling.scalalogic.discourse

trait DiscourseInterpreter[T] {

    /**
     * @param input: Input sentence to parse
     * @param discourseId: An identifier
     */
    final def interpret(input: String, discourseId: Option[String] = None, question: Boolean = false, verbose: Boolean = false): T = {
        val d = this.batchInterpretMultisentence(List(List(input)), discourseId.map(List(_)), question, verbose)
        require(d.length == 1)
        d.head match {
            case Some(r) => return r
            case None => throw new RuntimeException("Unable to interpret: '%s'".format(input))
        }
    }

    /**
     * @param input: Input sentences to parse as a single discourse
     * @param discourseId: An identifier
     */
    final def interpretMultisentence(input: List[String], discourseId: Option[String] = None, question: Boolean = false, verbose: Boolean = false): T = {
        val d = this.batchInterpretMultisentence(List(input), discourseId.map(List(_)), question, verbose)
        require(d.length == 1)
        d.head match {
            case Some(r) => return r
            case None => throw new RuntimeException("Unable to interpret: '%s'".format(input))
        }
    }

    /**
     * @param inputs: Input sentences to parse as individual discourses
     * @param discourseIds: Identifiers
     */
    final def batchInterpret(inputs: List[String], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[T]] = {
        val ds = this.batchInterpretMultisentence(inputs.map(List(_)), discourseIds, question, verbose)
        require(ds.length == inputs.length, "len(inputs) = %d, len(ds) = %d".format(inputs.length, ds.length))
        ds
    }

    /**
     * @param inputs: Input discourses to parse
     * @param discourseIds: Identifiers
     */
    def batchInterpretMultisentence(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[T]]

}
