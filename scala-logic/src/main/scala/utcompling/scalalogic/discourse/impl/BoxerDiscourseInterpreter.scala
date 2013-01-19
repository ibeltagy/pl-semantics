package utcompling.scalalogic.discourse.impl

import scala.collection.mutable.ListBuffer
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import opennlp.scalabha.util.FileUtils
import utcompling.scalalogic.base.expression.BaseExpression
import scala.collection.mutable.MapBuilder
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.call.Boxer
import utcompling.scalalogic.discourse.candc.call.Candc
import utcompling.scalalogic.discourse.candc.call.impl.BoxerImpl
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl

/**
 * An interface to the Boxer software
 * [a href='http://svn.ask.it.usyd.edu.au/trac/candc/wiki/boxer']http://svn.ask.it.usyd.edu.au/trac/candc/wiki/boxer[/a]
 */
class BoxerDiscourseInterpreter[T](
  private val boxerExpressionInterpreter: BoxerExpressionInterpreter[T] = new Boxer2DrtExpressionInterpreter(),
  private val candc: Candc = CandcImpl.findBinary(),
  private val boxer: Boxer = BoxerImpl.findBinary(),
  private val verbose: Boolean = false)
  extends DiscourseInterpreter[T] {

  override def batchInterpretMultisentence(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[T]] = {
    val newDiscourseIds = discourseIds.getOrElse((0 until inputs.length).map(_.toString).toList)
    require(inputs.length == newDiscourseIds.length)

    val candcArgs = Map[String, String](
      "--candc-printer" -> "boxer")
    val candcOut = this.candc.batchParseMultisentence(inputs, candcArgs, Some(newDiscourseIds), Some(if (question) "questions" else "boxer"), verbose = verbose)

    val boxerArgs = Map[String, String](
      "--box" -> "false",
      "--semantics" -> "drs",
      "--flat" -> "false",
      "--resolve" -> "true",
      "--elimeq" -> "true",
      "--format" -> "prolog",
      "--instantiate" -> "true")
    val boxerOut = this.boxer.callBoxer(candcOut, boxerArgs, verbose = verbose)

    val drsDict = this.parseBoxerOutput(boxerOut)
    return newDiscourseIds.map(drsDict.getOrElse(_, None)).toList
  }

  private def parseBoxerOutput(boxerOut: String): Map[String, Option[T]] = {
    val drsDict = new MapBuilder[String, Option[T], Map[String, Option[T]]](Map[String, Option[T]]())
    val singleQuotedRe = """^'(.*)'$""".r

    val lines = boxerOut.split("\n").iterator
    val IdLineRe = """^id\((\S+),\s*(\d+)\)\.$""".r
    val SemLineRe = """^sem\((\d+),$""".r
    for (line <- lines.map(_.trim)) {
      line match {
        case IdLineRe(discourseId, drsId) =>
          lines.next.trim match { case SemLineRe(drsId2) => require(drsId == drsId2, "%s != %s".format(drsId, drsId2)) }
          lines.next.trim match { case l if l.startsWith("[word(") => }
          lines.next.trim match { case l if l.startsWith("[pos(") => }
          lines.next.trim match { case l if l.startsWith("[") => }
          val drsInput = lines.next.trim.stripSuffix(").")

          val cleanDiscourseId = singleQuotedRe.findFirstMatchIn(discourseId).map(_.group(1)).getOrElse(discourseId)
          val parsed = this.parseOutputDrs(drsInput, cleanDiscourseId)
          drsDict += cleanDiscourseId -> Some(this.boxerExpressionInterpreter.interpret(parsed))
        case _ =>
      }
    }
    return drsDict.result
  }

  private def parseOutputDrs(drsString: String, discourseId: String): BoxerExpression = {
    return new BoxerExpressionParser(discourseId).parse(drsString)
  }

}
