package utcompling.scalalogic.discourse

trait DiscourseParser[T] {

    /**
     * Call the C&C binary with the given input.
     *
     * @param inputs: Input sentence to parse
     * @param discourseId: An identifier
     */
    final def parse(input: String, args: Map[String, String] = Map(), discourseId: Option[String] = None, model: Option[String] = None, verbose: Boolean = false): T = {
        val d = this.batchParseMultisentence(List(List(input)), args, discourseId.map(List(_)), model, verbose)
        require(d.length == 1)
        d.head match {
            case Some(r) => return r
            case None => throw new RuntimeException("Unable to parse: '%s'".format(input))
        }
    }

    /**
     * Call the C&C binary with the given input.
     *
     * @param inputs: Input sentences to parse as a single discourse
     * @param discourseId: An identifier
     */
    final def parseMultisentence(input: List[String], args: Map[String, String] = Map(), discourseId: Option[String] = None, model: Option[String] = None, verbose: Boolean = false): T = {
        val d = this.batchParseMultisentence(List(input), args, discourseId.map(List(_)), model, verbose)
        require(d.length == 1)
        d.head match {
            case Some(r) => return r
            case None => throw new RuntimeException("Unable to parse: '%s'".format(input))
        }
    }

    /**
     * Call the C&C binary with the given input.
     *
     * @param inputs: Input sentences to parse as individual discourses
     * @param discourseIds: Identifiers
     */
    final def batchParse(inputs: List[String], args: Map[String, String] = Map(), discourseIds: Option[Seq[String]] = None, model: Option[String] = None, verbose: Boolean = false): List[Option[T]] = {
        return this.batchParseMultisentence(inputs.map(List(_)), args, discourseIds, model, verbose)
    }

    /**
     * Call the C&C binary with the given input.
     *
     * @param inputs: Input discourses to parse
     * @param discourseIds: Identifiers
     */
    def batchParseMultisentence(inputs: List[List[String]], args: Map[String, String] = Map(), discourseIds: Option[Seq[String]] = None, model: Option[String] = None, verbose: Boolean = false): List[Option[T]]

}