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

	  
    val newGoal = BoxerPrs(goal.asInstanceOf[BoxerPrs].exps.map( e=> (process(e._1, Sts.hypothesis), e._2 )))
    val newAssumptions:List[WeightedExpression[BoxerExpression]] = assumptions.map({
      case HardWeightedExpression(BoxerPrs(exps), w) => HardWeightedExpression(BoxerPrs(exps.map( e=> 
        												(process(e._1, Sts.text), e._2 ))), w)
        												.asInstanceOf[WeightedExpression[BoxerExpression]];
      case _ => throw new RuntimeException("not reachable");
    })

    return delegate.prove(constants, declarations, evidence, newAssumptions, newGoal)
    
  }  
  
  //////////////////////////////////////////////////
  var sentence:String = "";
  def process(e:BoxerExpression, sentence: String) : BoxerExpression =
  {
  	this.sentence = sentence;
	removeTopic(  
		removeDanglingEqualities(
				removeTheres(
					corref(
						applySMerge(e)))));
  }
  /////////////////////////////////////////////
  var entities:Map[Set[String], (String, BoxerExpression)] = Map();
  var renamePair:Map[String, String] = Map();
  var listOfVars:List[String] = List()
  var toMerge:BoxerExpression = null
  var mergeTo:BoxerExpression = null
  def corref(e:BoxerExpression) : BoxerExpression =
  {
    /*currentSentenceIndex = 0;
    incrementVarName = 0;
    incrementIndices = 0;
    assert(!sentence.endsWith("\\."));
    sentences = sentence.split("\\.").toList;
    * 
    */
  	renamePair = Map()
  	entities = Map();
  	toMerge = null
  	mergeTo = null
    val result = correfRecursive(e);
  	assert (mergeTo == null)
  	assert (toMerge == null)
  	result;
  }
  def merge(e:BoxerExpression) : BoxerExpression =
  {
  	if(mergeTo == null)
  		return e
    e match {
      case BoxerMerge("smerge", first, second) => BoxerMerge("smerge", merge(first), merge(second))
      case BoxerMerge("merge", first, second) => {
      	if (e == mergeTo)
      	{
      		val merged = BoxerMerge("merge", first, BoxerDrs(List(), List(second, toMerge)))
      		toMerge = null
      		mergeTo = null
      		merged
      	}
      	else
      		e
      }
      case _ => e
    }
  }
  def correfRecursive(e:BoxerExpression) : BoxerExpression =
  {
    e match {
      case BoxerMerge("smerge", first, second) => {
      	//combine the two coreferring sentences into one
      	var modifiedFirst = correfRecursive(first)
      	val modifiedSecond = correfRecursive(second)
      	if (mergeTo != null)
      	{
      		assert (toMerge != null)
      		modifiedFirst = merge (modifiedFirst);
      	}
      	if (modifiedSecond  == null) // the toMerge should always be on Second  
      		modifiedFirst
      	else
      		BoxerMerge("smerge", modifiedFirst, modifiedSecond)
      }
      case BoxerMerge("merge", first, second) => {
      	  listOfVars = List();
      	  getVariables(first);
      	  val firstVars = listOfVars;
      	  listOfVars = List();
      	  getVariables(second);
      	  val secondVars = listOfVars;
      	  var intersection:List[String] = (firstVars.toSet & secondVars.toSet).toList
      	  var relatedVars:List[collection.mutable.Set[String]] = intersection.map(x=> (collection.mutable.Set[String]( (x))))
      	  if(intersection.size > 0)
      	  {
      	  	val rels = first.getRelations
      	  	var changed:Boolean = true
      	  	while (changed)
      	  	{
      	  		changed = false;
      	  		rels.forall(rel => {
      	  			relatedVars.forall(inter => {
      	  				val relVars = Set(rel.variable.name, rel.event.name);  
      	  				if (relVars.diff(inter).size == 1) //size == 0 means all variables already included before, size == 2 means no intersection
      	  				{
      	  					inter ++= relVars
      	  					changed = true
      	  				}
      	  				true
      	  			})
      	  		})
      	  	}
      	  	val preds = first.getPredicates
      	  	val clusteredPred = relatedVars.map(inter => {
      	  		preds.filter( p=> {
      	  			inter.contains(p.variable.name)	
      	  		}).map(_.name).toSet
      	  	})
      	  	//println (clusteredPred )
      	  	var renamed:BoxerExpression = null;
      	  	intersection.indices.foreach(intersectionIndex => {
      	  		val variable = intersection(intersectionIndex)
      	  		val variablePreds = clusteredPred(intersectionIndex)
      	  		var exists = entities.get(variablePreds);
				if (exists.isEmpty)
				{
					entities = entities  + ((variablePreds -> (variable, e)))
				}
				else
				{
					renamePair = Map(variable -> exists.get._1)
					mergeTo = exists.get._2  //Assume that there is a single base sentence.
					if (renamed == null)
						renamed  = second;
					renamed = renameVariables(renamed);
					renamePair.empty
					println ("<<<<<Coreference resolution done>>>>>")
				}
      	  	})
      	  	if (renamed == null)
      	  		e
      	  	else
      	  	{
      	  		toMerge = renamed
      	  		null
      	  	}
      	  }
      	  else e
      }
      case _ => e
    }
  }
  def getVariables(e:BoxerExpression):BoxerExpression = {
     e match {
    	case BoxerVariable(name) => listOfVars = listOfVars :+ ((name)); e
    	case _ => e.visitConstruct(getVariables)  
     }
  }
  def renameVariables(e:BoxerExpression):BoxerExpression = {
	e match {
		case BoxerVariable(name) => BoxerVariable(renamePair.getOrElse(name, name))
		case _ => e.visitConstruct(renameVariables)
	}
  }
  ////////////////////////////////////////////////////
  //change indecies to match the sentence. 
  //change variables names to introduce new entities for each sentence. 
  //No correference resolution between sentences
  //Assume periods between sentences look like : SENTENCE 1 . Sentence 2 . Sentence 3
  var currentSentenceIndex:Int = 0;
  var incrementVarName:Int = 0  
  var incrementIndices:Int = 0
  var sentences:List[String] = List();
  def applySMerge(e:BoxerExpression) : BoxerExpression =
  {
    currentSentenceIndex = 0;
    incrementVarName = 0;
    incrementIndices = 0;
    assert(!sentence.endsWith("\\."));
    sentences = sentence.split("\\.").toList;
    applySMergeRecursive(e);
  }
  def applySMergeRecursive(e:BoxerExpression) : BoxerExpression =
  {
    e match {
      case BoxerMerge("smerge", first, second) => BoxerMerge("smerge", applySMergeRecursive(first), applySMergeRecursive(second))
      case _ => {
      	var changedE = increment (e);
      	changedE = increment2 (changedE);
      	incrementVarName  = incrementVarName + countEntities(e)
      	assert (sentences.size > currentSentenceIndex);
      	incrementIndices = incrementIndices + sentences(currentSentenceIndex).trim().split("\\s").size + 1 /*for the period between the two sentences.*/;
      	currentSentenceIndex = currentSentenceIndex + 1;
      	changedE
      }
    }
  }
  def countEntities(e:BoxerExpression):Int = {
     e match {
    	case BoxerDrs(refs, conds) => return refs.size + conds.map(countEntities).reduce(_ + _)
    	case _ => e.visit(countEntities, (parts: List[Int]) => parts.reduce(_ + _), 0)  
     }
  }
  def increment2(e:BoxerExpression):BoxerExpression = {
	e match {
		case BoxerVariable(name) => BoxerVariable("" + name.charAt(0) + (incrementVarName + name.substring(1).toInt))
		case _ => e.visitConstruct(increment2)
	}
  }
  def increment(e:BoxerExpression):BoxerExpression = {
	e match {
		case BoxerDrs(refs, conds) => BoxerDrs(refs.map(r => (increment(r._1), r._2)), conds.map(increment))  
		case BoxerEq(discId, indices, first, second) => BoxerEq(discId, increment(indices), first, second) 
		case BoxerNamed(discId, indices, variable, name, typ, sense) => BoxerNamed(discId, increment(indices), variable, name, typ, sense)
		case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discId, increment(indices), variable, name, pos, sense)
		case BoxerProp(discId, indices, variable, drs) => BoxerProp(discId, increment(indices), variable, increment(drs))
		case BoxerRel(discId, indices, event, variable, name, sense) =>  BoxerRel(discId, increment(indices), event, variable, name, sense)
		case BoxerCard(discId, indices, variable, num, typ) => BoxerCard(discId, increment(indices), variable, num, typ)
		case BoxerDate(indicesPol, pol, indicesYear, year, indicesMonth, month, indicesDay, day) => 
				BoxerDate(increment(indicesPol), pol, increment(indicesYear), year, increment(indicesMonth), month, increment(indicesDay), day)
		case BoxerImp(discId, indices, first, second) => BoxerImp(discId, increment(indices), increment(first), increment(second))
		case BoxerNot(discId, indices, drs) => BoxerNot(discId, increment(indices), increment(drs))
		case BoxerOr(discId, indices, first, second) => BoxerOr(discId, increment(indices), increment(first), increment(second))
		case BoxerTimex(discId, indices, variable, timeExp) => BoxerTimex(discId, increment(indices), variable, increment(timeExp))
		case _ => e.visitConstruct(increment)
	}
  }
  def increment(e:List[BoxerIndex]):List[BoxerIndex] = {
  	return e.map(i => BoxerIndex(i.sentIndex + currentSentenceIndex, i.wordIndex + incrementIndices))
  }

  /////////////////////////////////////////
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
