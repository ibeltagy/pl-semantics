package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import scala.collection.mutable.SetBuilder
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import org.apache.commons.logging.LogFactory
import support.HardWeightedExpression
import utcompling.mlnsemantics.run.Sts
import opennlp.scalabha.util.FileUtils
import scala.math

class EditDRSTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {  
  
  private val LOG = LogFactory.getLog(classOf[EditDRSTheoremProver])

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Seq[Double] = {

	  
    val newGoal = BoxerPrs(goal.asInstanceOf[BoxerPrs].exps.map( e=> (process(e._1), e._2 )))
    val newAssumptions:List[WeightedExpression[BoxerExpression]] = assumptions.map({
      case HardWeightedExpression(BoxerPrs(exps)) => HardWeightedExpression(BoxerPrs(exps.map( e=> 
        												(process(e._1), e._2 ))))
        												.asInstanceOf[WeightedExpression[BoxerExpression]];
      case _ => throw new RuntimeException("not reachable");
    })

    return delegate.prove(constants, declarations, evidence, newAssumptions, newGoal)
    
  }  
  
  //////////////////////////////////////////////////
  
  def process(e:BoxerExpression) : BoxerExpression =
  {
	removeTopic(  
	  removeDanglingEqualities(
	      removeTheres(e)));
  }
  ////////////////////////////////////////////////////
  private var topicPreds:Seq[BoxerRel] = null;
  private var varMap:Map[BoxerVariable, BoxerVariable] = null;  
  
  def removeTopicAndRenameVars(e:BoxerExpression) : BoxerExpression =
  {
    e match {
      case BoxerDrs(ref, cond) => BoxerDrs(ref.filterNot(r=>{varMap.keys.contains(r._2)}), 
          cond.filterNot(c=> {topicPreds.contains(c) }).map(removeTopicAndRenameVars))
      case BoxerVariable(v) => varMap.getOrElse(e.asInstanceOf[BoxerVariable], e)
      case _ => e.visitConstruct(removeTopicAndRenameVars)
    }
  }
  
  
  def removeTopic(e:BoxerExpression) : BoxerExpression =
  {
    topicPreds = Seq();
    varMap = Map();
    topicPreds = e.getRelations.flatMap(rel =>{
	    rel match
	    {
	      case BoxerRel(discId, indices, event, variable, "topic", sense) => {
	    	varMap = varMap + (event -> variable)  
	        Some(rel) 
	      }
	      case _  => None
	    }
	})
	return removeTopicAndRenameVars (e);
  }
  
  ////////////////////////////////////////////////////
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
