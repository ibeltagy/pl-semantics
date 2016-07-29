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
import scala.collection.mutable.ListBuffer

class EditDRSTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {  
  
  private val LOG = LogFactory.getLog(classOf[EditDRSTheoremProver])
  var isProcessingGoal = true;
  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Seq[Double] = {

    isProcessingGoal = true;
    val newGoal = BoxerPrs(goal.asInstanceOf[BoxerPrs].exps.map( e=> (process(e._1, Sts.hypothesis), e._2 )))
    isProcessingGoal = false;
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
		addGroupHeadPred(
			extendPlaceholder(
				setNamedEntities(
					replaceCardName(
						removeTopic(
							removeDanglingEqualities(
								removeTheres(
									corref(
										applySMerge(
											removeSingleQuote(e))))))))));
  }

  /////////////////////////////////////////////
  def addGroupHeadPred(e:BoxerExpression):BoxerExpression = {
   e match {
		case BoxerDrs(refs, conds) => { 
			var expDiscId:String = ""
			val groupHeads = conds.flatMap(cond => cond match 
			{
				case BoxerRel(discId, indices, event, variable, "subset_of", sense) => expDiscId = discId;  Some(variable);
				case _ => None
			}).toSet
			val extraPreds = groupHeads.map (v => BoxerPred(expDiscId, List(), v, "group_head", "r", 0))
			BoxerDrs(refs, conds.map(addGroupHeadPred) ++ extraPreds)
		}
//case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discId, indices, variable, name, pos, sense)
//case BoxerRel(discId, indices, event, variable, name, sense) =>  BoxerRel(discId, indices, event, variable, name, sense)

      case _ => e.visitConstruct(addGroupHeadPred)
   }
  }

  def extendPlaceholder(e:BoxerExpression):BoxerExpression = 
  {
    if (isProcessingGoal)
    {
    	//if the predicate @placeholder is sharing the same variable with other noun or verb 
    	//move it to a new variable with a relation
    	
    	val placeholderPred = e.getPredicates.filter( _.name.contains("@placeholder"))
    	//It is possible to have hyp with multiple placeholder predicates. 
    	//require (placeholderPred.length <= 1, "More than one predicate is named @placeholder in " + e )
    	if (placeholderPred.length > 0)
    	{
    		//all of them should have the same variable because "setNamedEntities" is called before it
    		val varName = placeholderPred.head.variable.name;
    		
    		val otherPreds = e.getPredicates.filter( p => p.variable.name == varName && !p.name.contains("@placeholder"))
    		if (!otherPreds.isEmpty)
    		{
    			return extendPlaceholderRecursive(e)
    		}
    	}
    }
    return e
  }
  def extendPlaceholderRecursive(e:BoxerExpression):BoxerExpression =
  {
	e match {
      case BoxerPred(discId, indices, variable, name, pos, sense) => {
        if (name.contains("@placeholder"))
        {
          val newVar = BoxerVariable("x900");
          val rel = BoxerRel(discId, indices, variable, newVar, "rel", 0);
          val pred = BoxerPred(discId, indices, newVar, name, pos, sense)
          val refs = List((List[BoxerIndex](), newVar)) ;
          val drs = BoxerDrs(refs, List(rel, pred));
          Sts.qaEntities = Sts.qaEntities + ("@placeholder" -> Set( ("h" + FindEventsProbabilisticTheoremProver.newName(newVar.name))) )
          drs
        }else e
      }
      case _ => e.visitConstruct(extendPlaceholderRecursive)
	}
  }
  var entityVarMap: collection.mutable.Map[String, collection.mutable.ListBuffer[String]] =  collection.mutable.Map()
  def setNamedEntities(inputE:BoxerExpression):BoxerExpression = 
  {
  	if (Sts.qaRightAnswer == "") //if not QA task
  		return inputE
  		
  	var e = inputE;
	var preds = e.getPredicates;
	if (preds.filter(_.name == "@placeholder").size == 0 && isProcessingGoal && Sts.qaRightAnswer != "") //QA question without a placeholder
	{
		val newVar = BoxerVariable("x1");
		val pred = BoxerPred("h", List(), newVar, "@placeholder", "n", 0)
		val refs = List((List[BoxerIndex](), newVar)) ;
		e = BoxerDrs(refs, List(pred, e));
		preds = e.getPredicates();
	}

	preds.foreach(p => {
		if(Sts.qaEntities.contains(p.name))
			if(!isProcessingGoal || p.name == "@placeholder") //do not add entities from goal other than the placeholder entity
				Sts.qaEntities = Sts.qaEntities + ( p.name -> Sts.qaEntities(p.name).+("h" + FindEventsProbabilisticTheoremProver.newName(p.variable.name)) )
	})
	
	val exp = if (isProcessingGoal)
	{
		//make sure I have only one entity for the query. This should be the case by default, but mis-parses could result into multiple placeholders
		renamePair.clear()
		var placeholderEntities = Sts.qaEntities("@placeholder").toList
		placeholderEntities.foreach(y => renamePair = renamePair + (y -> placeholderEntities.head))
		Sts.qaEntities = Sts.qaEntities + ( "@placeholder" -> Set().+(placeholderEntities.head) )
		renameVariables(e)
	}
	else
	{
		Sts.qaEntities.foreach( e1 => {
			Sts.qaEntities.foreach( e2 => {
				if (e1._1 != e2._1)
				{
					if (e1._2.intersect(e2._2).size > 0)
						LOG.error ("Nonezero Overlap: " + e1._1 + " - " + e2._1)
				}
			})	
		})
		e
	}
	LOG.trace("Entities and variables: " +  Sts.qaEntities)
	exp
	
	/*
  	entityVarMap.clear();
    findCorefEntities(e)
    renamePair.clear()
	
    entityVarMap.foreach(x => 
      {
        val entityName = x._1
        val varList = x._2
        varList.foreach(y => renamePair = renamePair + (y -> varList.head))
        assert(Sts.qaEntities.contains(entityName))
        if (!isProcessingGoal || entityName == "@placeholder") //do not add variables from goal, only the variable of @placeholder because it is neede later 
        												//to form the PSL query
        	Sts.qaEntities = Sts.qaEntities + (entityName -> ("h" + FindEventsProbabilisticTheoremProver.newName(varList.head)))
      })
    //println("++++" + renamePair)

    

    //the "dep" baseline works better with entities not renamed
    //use the opts.coref option to enable or disable renaming 
    //but always rename variables for goal and for running with PSL
    if ((Sts.opts.coref && Sts.opts.baseline != "full") || Sts.opts.baseline == "full" || isProcessingGoal)
    	renameVariables(e)
    else e
    */
  }

  def findCorefEntities(e:BoxerExpression):BoxerExpression = {
	e match {
    	case BoxerPred(discId, indices, variable, name, pos, sense) => {
    	  var predName = name
    	  
    	  //Quotes are removed in the function removeSingleQuote
    	  //if (predName.startsWith("'") && predName.endsWith("'"))
    	  //	predName = predName.substring(1, predName.length()-1);
    	  
    	  if (Sts.qaEntities.contains(predName))
    	  {
    	    //it is an entity
    	    if (!entityVarMap.contains(predName))
    	      entityVarMap.put(predName, collection.mutable.ListBuffer())
    	    val varList = entityVarMap.get(predName).get
    	    varList.append(variable.name);
    	  }
    	}
		case _ => e.visitConstruct(findCorefEntities)
	}
	e
  }
    
  /////////////////////////////////////////////
  val numbers:List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve");
  def replaceCardName(e:BoxerExpression):BoxerExpression = {
	e match {
		case BoxerNamed(discId, indices, variable, name, typ, sense) => BoxerPred(discId, indices, variable, name, "n", sense)
		case BoxerCard(discId, indices, variable, num, typ) => 
		try
		{
			if(num.toInt > numbers.size)
				BoxerPred(discId, indices, variable, "card_" + num, "n", 0) 
			else
				BoxerPred(discId, indices, variable, numbers(num.toInt), "n", 0)
		}
		catch 
		{
			case _ : Exception => BoxerPred(discId, indices, variable, "card_" + num, "n", 0)
		}
		//case BoxerDate(indicesPol, pol, indicesYear, year, indicesMonth, month, indicesDay, day) => 
		//		BoxerDate(increment(indicesPol), pol, increment(indicesYear), year, increment(indicesMonth), month, increment(indicesDay), day)
		//case BoxerTimex(discId, indices, variable, timeExp) => BoxerTimex(discId, increment(indices), variable, increment(timeExp))
		case _ => e.visitConstruct(replaceCardName)
	}
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
  	val result = e //correfRecursive(e);
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
				var toBeAddedToEntities:Map[Set[String], (String, BoxerExpression)] = Map();// to avoid coreferences within the same sentence
      	  	intersection.indices.foreach(intersectionIndex => {
      	  		val variable = intersection(intersectionIndex)
      	  		val variablePreds = clusteredPred(intersectionIndex)
      	  		var exists = entities.get(variablePreds);
				if (exists.isEmpty)
				{
					toBeAddedToEntities = toBeAddedToEntities  + ((variablePreds -> (variable, e)))
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
				entities = entities ++ toBeAddedToEntities;
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
    	case BoxerDrs(refs, conds) => return refs.size + (conds.map(countEntities) :+ 0 ).reduce(_ + _); 
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
	 if (!Sts.opts.removeEq)
		return e;
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
  ////////////////////////////
  def removeSingleQuote(e:BoxerExpression) : BoxerExpression =
  {
	e match
	{
		case BoxerPred(discId, indices, variable, name, pos, sense) =>
		{
			var newName = name;
			if (name.head == ''' && name.last == ''' && name.length() > 2)
				newName = name.substring(1, name.length()-1);
			BoxerPred(discId, indices, variable, newName, pos, sense)
		}
		case _ => e.visitConstruct(removeSingleQuote)
	}
  }
}
