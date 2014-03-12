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
    
    hypEx = BoxerPrs(hypEx.asInstanceOf[BoxerPrs].exps.map( e=> (removeThere(e._1), e._2 )))
    txtEx = BoxerPrs(txtEx.asInstanceOf[BoxerPrs].exps.map( e=> (removeThere(e._1), e._2 )))
    
    val constants:Map[String, Set[String]] = Map();
    val declarations:Map[BoxerExpression, Seq[String]] = Map();
    val evidence = List();
    val assumptions = List(HardWeightedExpression(txtEx))
    val goal = hypEx
    probabilisticTheoremProver.prove(constants, declarations, evidence, assumptions, goal)
  }
  
  def removeThere(e:BoxerExpression) : BoxerExpression = 
  {
	  e match 
	  {
	    case BoxerDrs(ref, cond) if(cond.length == 2)=> {
	      var thereVar:BoxerVariable = null;
	      var negatedDrs:BoxerDrs = null;
	      var eqIndex = -1;
	      var notDiscId =  "";
	      var notIndices: List[BoxerIndex] = null;
	      (cond.head, cond.last) match 
	      {
	        case (BoxerPred(discId1, indices1, variable1, "there", pos, sense), 
	              BoxerProp(discId2, indices2, variable2, BoxerDrs(ref, List(BoxerNot(discId3, indices3, drs)))))
	              =>thereVar = variable1;  negatedDrs = drs.asInstanceOf[BoxerDrs];	notDiscId = discId3 ; notIndices = indices3
	        case (BoxerProp(discId2, indices2, variable2, BoxerDrs(ref, List(BoxerNot(discId3, indices3, drs)))),
	              BoxerPred(discId1, indices1, variable1, "there", pos, sense))
	              =>thereVar = variable1;  negatedDrs = drs.asInstanceOf[BoxerDrs];	notDiscId = discId3 ; notIndices = indices3             
	        case _ => return e;
	      }
	      negatedDrs.conds.indices.foreach(i=>
	      {
	          negatedDrs.conds(i) match 
	          {
	            case BoxerEq(discId, indices, first, second) => if(first == thereVar || second == thereVar) eqIndex = i;
	            case _ => 
	          }
	      })
	      if(eqIndex != -1)
			{
				println ("<<<<< THERE removed >>>>>")
				return BoxerNot(notDiscId, notIndices, BoxerDrs(negatedDrs.refs, negatedDrs.conds.filterNot(_ == negatedDrs.conds(eqIndex))))
			}
	    }
	    case _ => return e;
	  }
	  return e;
  }
  
}
