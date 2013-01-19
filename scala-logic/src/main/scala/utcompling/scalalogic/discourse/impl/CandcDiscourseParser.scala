package utcompling.scalalogic.discourse.impl

import utcompling.scalalogic.discourse.DiscourseParser
import utcompling.scalalogic.discourse.candc.call.Candc
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.candc.parse.output.CandcOutputInterpreter
import utcompling.scalalogic.discourse.candc.parse.output.impl.CandcOutputInterpreterImpl

class CandcDiscourseParser[T](
    private val candc: Candc = CandcImpl.findBinary(),
    private val candcOutputInterpreter: CandcOutputInterpreter[T] = new CandcOutputInterpreterImpl(),
    private val defaultArgs: Map[String, String] = Map())
    extends DiscourseParser[T] {

    override def batchParseMultisentence(inputs: List[List[String]], args: Map[String, String] = Map(), discourseIds: Option[Seq[String]] = None, model: Option[String] = None, verbose: Boolean = false): List[Option[T]] = {
        val newDiscourseIds = discourseIds.getOrElse((0 until inputs.length).map(_.toString))
        val defaultArgs = Map[String, String](
            "--candc-printer" -> "grs")
        val candcOut = this.candc.batchParseMultisentence(inputs, (this.defaultArgs ++ defaultArgs ++ args), Some(newDiscourseIds), model, verbose)
        val discourseDict = this.candcOutputInterpreter.interpret(candcOut)
        return newDiscourseIds.map(discourseDict.getOrElse(_, None)).toList
    }

}