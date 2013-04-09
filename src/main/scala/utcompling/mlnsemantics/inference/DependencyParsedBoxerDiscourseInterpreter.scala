package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse._
import utcompling.scalalogic.discourse.impl._
import utcompling.scalalogic.discourse.candc.boxer._
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.scalalogic.drt.expression._
import utcompling.scalalogic.top.expression._
import utcompling.scalalogic.discourse.candc.call._
import utcompling.scalalogic.discourse.candc.call.impl._
import utcompling.scalalogic.discourse.candc.parse.output.impl._
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import dhg.depparse._
import expression.BoxerExpression

/**
 * Discourse Interpreter that simply interprets pre-parsed strings
 */
class DependencyParsedBoxerDiscourseInterpreter[T](
    depParser: DepParser)
  extends DiscourseInterpreter[T] {

  /**
   * Hook to which all interpret calls delegate.
   */
  override def batchInterpretMultisentence(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[T]] = {
    val newDiscourseIds = discourseIds.getOrElse((0 until inputs.length).map(_.toString).toList)
    
    require(inputs.length == newDiscourseIds.length)
    val graphs = inputs.map(t => depParser.apply(t).head)
    val box = graphs.map(g =>  Some(predicatesToBoxerExpression(g.logic)))

    return box;
    /*
    (interpretations zipSafe newDiscourseIds)
      .map {
        case (Some(drsString), discourseId) =>
          val lineParser = new BoxerExpressionParser(discourseId)
          Some(boxerExpressionInterpreter.interpret(lineParser.parse(drsString)))
        case (None, _) => None
      }
      .toList
      */
  }
  
  protected def predicatesToBoxerExpression(preds: List[Predicate]): T;
  
  def predicatesToBoxerExpression(preds: List[Predicate]): T = {
    return DrtVariableExpression(Variable("Var"))
  }

}


