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
    
    hypEx = BoxerPrs(hypEx.asInstanceOf[BoxerPrs].exps.map( e=> (removeDanglingEqualities(removeTheres(e._1)), e._2 )))
    txtEx = BoxerPrs(txtEx.asInstanceOf[BoxerPrs].exps.map( e=> (removeDanglingEqualities(removeTheres(e._1)), e._2 )))
    
    //return Seq(0)
    
    val constants:Map[String, Set[String]] = Map();
    val declarations:Map[BoxerExpression, Seq[String]] = Map();
    val evidence = List();
    val assumptions = List(HardWeightedExpression(txtEx))
    val goal = hypEx
    probabilisticTheoremProver.prove(constants, declarations, evidence, assumptions, goal)
  }
   
  private var thereToBeRemoved:BoxerPred = null;
  private var eqToBeRemoved:BoxerEq = null;
  private var varToBeRemoved:String = null;
  def countRef(e:BoxerExpression) : Int =
  {
    e match {
		case BoxerAlfa(variable, first, second) =>  countRef(variable) + countRef(first) + countRef(second)
		case BoxerDrs(refs, conds) => 
		  val l1 = refs.map(r=>{countRef(r._2)})
		  val l2 = conds.map(c=>{countRef(c)})
		  (l1 ++ l2).foldLeft(0)( _ + _) 
		case BoxerEq(discId, indices, first, second) => countRef(first) + countRef(second)
		case BoxerNamed(discId, indices, variable, name, typ, sense) => countRef(variable);
		case BoxerPred(discId, indices, variable, name, pos, sense) => countRef(variable);
		case BoxerProp(discId, indices, variable, drs) => countRef(variable) + countRef(drs);
		case BoxerRel(discId, indices, event, variable, name, sense) =>  countRef(variable) + countRef(event);
		case BoxerCard(discId, indices, variable, num, typ) => countRef(variable);
		case BoxerVariable(v) => if(v == varToBeRemoved) 1 else 0;
		case _ => e.visit(countRef, (parts: List[Int]) => parts.reduce(_ + _), 0)
    }  
  }
  
  def removeThere(e:BoxerExpression) : BoxerExpression =
  {
    e match {
      case BoxerDrs(ref, cond) => BoxerDrs(ref.filterNot(r=>{r._2.name == varToBeRemoved}), 
          cond.filterNot(c=> {c == thereToBeRemoved || c == eqToBeRemoved}).map(removeThere))
      case _ => e.visitConstruct(removeThere)
    }
  }
  
  def removeTheres(e:BoxerExpression) : BoxerExpression = 
  {
    var exp = e;
	val theres = e.getPredicates.flatMap(pred =>{
	    pred match
	    {
	      case BoxerPred(discId, indices, variable, "there", pos, sense) => Some((pred, variable.name))
	      case _  => None
	    }
	})
	val eqs = e.getEqualities.flatMap(eq =>{
	    eq match
	    {
	      case BoxerEq(discId, indices, first, second) => Some((eq, first.name, second.name))
	    }
	})
	
	theres.foreach(there =>
	{
		eqs.foreach(eq=>{
		  if(there._2 == eq._2 || there._2 == eq._3)
		  {
			thereToBeRemoved = there._1
			eqToBeRemoved = eq._1
			varToBeRemoved = there._2;
			val cnt = countRef(exp);
			if(cnt != 3)
			  println("<<<<< THERE ERROR, cnt = "+cnt+" >>>>>")
			else
			{    
			  exp = removeThere(exp);
			  println ("<<<<< THERE removed >>>>>")
			}
		  }
		})
	})
	return exp;
  }
/////////////////////////////////////////
  def countPredRef(e:BoxerExpression) : Int =
  {
    e match {
		case BoxerAlfa(variable, first, second) =>  /*countPredRef(variable) +*/ countPredRef(first) + countPredRef(second)
		case BoxerDrs(refs, conds) => 
		  //val l1 = refs.map(r=>{countPredRef(r._2)})
		  val l2 = conds.map(c=>{countPredRef(c)})
		  (/*l1 ++ */ l2).foldLeft(0)( _ + _) 
		case BoxerEq(discId, indices, first, second) => countPredRef(first) + countPredRef(second)
		case BoxerNamed(discId, indices, variable, name, typ, sense) => countPredRef(variable);
		case BoxerPred(discId, indices, variable, name, pos, sense) => countPredRef(variable);
		case BoxerProp(discId, indices, variable, drs) => /*countPredRef(variable) +*/ countPredRef(drs);
		case BoxerRel(discId, indices, event, variable, name, sense) =>  countPredRef(variable) + countPredRef(event);
		case BoxerCard(discId, indices, variable, num, typ) => countPredRef(variable);
		case BoxerVariable(v) => if(v == varToBeRemoved) 1 else 0;
		case _ => e.visit(countPredRef, (parts: List[Int]) => parts.reduce(_ + _), 0)
    }  
  }
  
  def removeDanglingEquality(e:BoxerExpression) : BoxerExpression =
  {
    e match {
      case BoxerDrs(ref, cond) => BoxerDrs(ref.filterNot(r=>{r._2.name == varToBeRemoved}), 
          cond.filterNot(c=> { c == eqToBeRemoved}).map(removeDanglingEquality))
      case _ => e.visitConstruct(removeDanglingEquality)
    }
  }
  
  def removeDanglingEqualities(e:BoxerExpression) : BoxerExpression = 
  {
    var exp = e;
	val eqs = e.getEqualities.flatMap(eq =>{
	    eq match
	    {
	      case BoxerEq(discId, indices, first, second) => Some((eq, first.name, second.name))
	    }
	})
	eqs.foreach(eq=>
	{
		eqToBeRemoved = eq._1
		varToBeRemoved = eq._2;  //first equality variable
		val cntAllRef = countRef(exp);
		val cntPredRef = countPredRef(exp);
		if(cntPredRef == 1 && cntAllRef == 2 )
		{
		  exp = removeDanglingEquality(exp);
		  println ("<<<<< EQ removed >>>>>")
		}
		else if(cntPredRef == 1 && cntAllRef != 2 )
		{
		  println("<<<<< EQ REMOVE ERROR on Variable " + varToBeRemoved + ", countAllRef = "+cntAllRef+" >>>>>")
		}
		else if(cntPredRef == 0 )
		{
		  throw new RuntimeException("not reachable");
		}
		else 
		{
			varToBeRemoved = eq._3; //second equality variable
			val cntAllRef = countRef(exp);
			val cntPredRef = countPredRef(exp);
			if(cntPredRef == 1 && cntAllRef == 2 )
			{
			  exp = removeDanglingEquality(exp);
			  println ("<<<<< EQ removed >>>>>")
			}
			else if(cntPredRef == 1 && cntAllRef != 2 )
			{
				println("<<<<< EQ REMOVE ERROR on Variable " + varToBeRemoved + ", countAllRef = "+cntAllRef+" >>>>>")
			}
			else if(cntPredRef == 0 )
			{
			  throw new RuntimeException("not reachable");
			}
				
		}
	})
	return exp;
  }
  
}
