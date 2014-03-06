package utcompling.mlnsemantics.inference

import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import opennlp.scalabha.util.FileUtils.pathjoin
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import utcompling.scalalogic.inference.TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.UnnecessarySubboxRemovingBoxerExpressionInterpreter
import utcompling.scalalogic.drt.expression.parse.DrtLogicParser
import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.FolApplicationExpression
import utcompling.scalalogic.fol.expression.FolVariableExpression
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SetBuilder
import utcompling.scalalogic.fol.expression.FolAtom
import org.apache.log4j.Logger
import org.apache.log4j.Level
import utcompling.mlnsemantics.inference.support._
import utcompling.scalalogic.discourse.DiscourseInterpreter
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.run.Sts

class TextualTheoremProver(
  discourseIterpreter: DiscourseInterpreter[BoxerExpression],
  probabilisticTheoremProver: ProbabilisticTheoremProver[BoxerExpression]) {

  private val LOG = LogFactory.getLog(classOf[TextualTheoremProver])
  
  def prove(text: String, hyp: String): Seq[Double] =
    prove(List(text), List(hyp))

  def prove(text: List[String], hyp: List[String]): Seq[Double] = {

    LOG.trace(text)
    LOG.trace(hyp)
    val List(t, h) = discourseIterpreter.batchInterpretMultisentence(List(text, hyp), Some(List("t", "h")), false, false)

	 var parseError = false;
    var txtEx  = (t match {
      case Some(txt) => txt;
      case _ => {
        println ("Parsing text failed. Return -2");
		  parseError = true;
		  null;
      }
    })
    
    var hypEx  = (h match {
      case Some(txt) => txt;
      case _ => {
        println ("Parsing hypothesis failed. Return -2");
		  parseError = true;
		  null;
      }
    })

	 if (parseError)
	 {
	   if(Sts.opts.task == "sts")
			  return Seq.fill(Sts.opts.kbest * Sts.opts.kbest * 2)(-2);
	   else  if (Sts.opts.task == "rte" && Sts.opts.withNegT && Sts.opts.softLogicTool != "psl")
   		  return Seq.fill(Sts.opts.kbest * Sts.opts.kbest * 2)(-2); //check GivenNotTextProbabilisticTheoremProver for details
	   else return Seq.fill(Sts.opts.kbest * Sts.opts.kbest)(-2);
	 }

    
    val constants:Map[String, Set[String]] = Map();
    val declarations:Map[BoxerExpression, Seq[String]] = Map();
    val evidence = List();
    val assumptions = List(HardWeightedExpression(txtEx))
    val goal = hypEx
    probabilisticTheoremProver.prove(constants, declarations, evidence, assumptions, goal)

  }
}
