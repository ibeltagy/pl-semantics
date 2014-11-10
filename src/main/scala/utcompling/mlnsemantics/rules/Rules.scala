package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.run.Sts
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import utcompling.mlnsemantics.datagen.Lemmatize
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.mlnsemantics.datagen.Lemmatize
import scala.util.control.Breaks._
import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.MutableList
import scala.collection.mutable.Queue


class Rules {
}

class RuleType {
}

object RuleType extends Enumeration {
	  type RuleType = Value
	  val Implication = Value("Implication") 
	  val DoubleImplication = Value("DoubleImplication")
	  val BackwardImplication = Value("BackwardImplication")
	  val Opposite = Value("Opposite")
  }

object Rules {

  private val LOG = LogFactory.getLog(classOf[Rules])

  //THIS IS WRONG: no token should be removed
  //val tokensToRemoveFromRules = List("a", "an", "the")
  val tokensToRemoveFromRules = List()

  /**
   * Convert paraphrase rules in text format to FOL.
   * Rules have the format: <id> TAB <text_phrase> TAB <hypo_phrase> TAB <sim_score>
   */
  
  private def simplePrintPred(exp:BoxerExpression):String = {
	exp match {
		case BoxerPred(discId, indices, variable, name, pos, sense) => name + "-" + pos + "-" + variable.name ;
		case BoxerRel(discId, indices, event, variable, name, sense) =>name + "-r-" + variable.name + "-" + event.name;
	}
  }
  private def simplerPrintPred(exp:BoxerExpression):String = {
	exp match {
		case BoxerPred(discId, indices, variable, name, pos, sense) => pos + "-" + variable.name ;
		case BoxerRel(discId, indices, event, variable, name, sense) =>"-r-" + variable.name + "-" + event.name;
	}
  }
  private def simplestPrintPred(exp:BoxerExpression):String = {
	exp match {
		case BoxerPred(discId, indices, variable, name, pos, sense) => pos ;
		case BoxerRel(discId, indices, event, variable, name, sense) =>"r";
	}
  }
  
  private def printExpPattern(exps:Set[BoxerExpression]):String = {
  	//var varsTypes: Set[(String, String)] = Set();
  	var varsPOS: Map[String, String] = Map();
  	var posIndexMap :Map[String, Int] = Map(("v"->4), ("n"->3), ("a"->2), ("r"->1))
  	def addVarPos(varname:String, pos:String) = 
  	{
		val resPos = varsPOS.getOrElse(varname, pos)
		val resPosIndex = posIndexMap(resPos)
		val posIndex = posIndexMap(pos)			
		val newPos = (if(resPosIndex >= posIndex)
			resPos;
		else
			pos);
		(varname ->newPos)
  	}
  	exps.foreach( exp=> exp match {
		case BoxerPred(discId, indices, variable, name, pos, sense) => {
			//varsTypes = varsTypes + ((variable.name, pos)) ;
			varsPOS = varsPOS + addVarPos(variable.name, pos);
		}
		case BoxerRel(discId, indices, event, variable, name, sense) =>{
			//varsTypes = varsTypes + ((variable.name, "r")) + ((event.name, "r"))
			varsPOS = varsPOS + addVarPos(variable.name, "r");
			varsPOS = varsPOS + addVarPos(event.name, "r");			
		}
	}
	)
	val sortedVarsPOS = varsPOS.toList.sortBy(_._2);
	sortedVarsPOS.indices.map(idx => "v" + idx +"-"+sortedVarsPOS(idx)._2).mkString(",")
  }
  
  //sort variables following the chain of relations
  //rename variables to x1, x2, ...
  //return a pattern describing the expression consist of POS tags of variables ordered
  //POS tags are: 1)v: verb, 2)n: noun or adjectave (all "a" are renamed to "n"), 3)r: relation variable.
  //Return expression with variables renames, and pattern as strng
  private def sortVarsRenameVarsGetPattern(exps:Set[BoxerExpression]):(Set[BoxerExpression], String)= 
  {
  	if(exps.size == 0) //empty expression 
  		return (exps, "");
	//1)Build the sorting order of the variables using relations  
	var singleVariable:String = null;
  	var fromTo:Set[(String, String)] = Set()
	exps.foreach( exp=> exp match {
		case BoxerPred(discId, indices, variable, name, pos, sense) => singleVariable = variable.name;
		case BoxerRel(discId, indices, event, variable, name, sense) =>
			fromTo   = fromTo + ((event.name, variable.name))
	})

  	var startPoint:scala.collection.immutable.Set[String]  = null;
  	var (from, to) = fromTo.unzip;
  	if(fromTo.size == 0 /*special case, rules with no relations*/)
  	{
  		startPoint = scala.collection.immutable.Set((singleVariable));
  	}
  	else
  	{
		startPoint = (from.toSet -- to.toSet);
		if (startPoint.size == 0 )
		{
			LOG.debug("circular rule " + exps);
			startPoint = scala.collection.immutable.Set((from.toList.sortBy(x=>x)).head);
		}	
  	}
	var queue = Queue[String]();
	var visitedSorted = MutableList[String]();
	val startPointsSorted = startPoint.toList.sortBy(x=>x);
	startPointsSorted.foreach(queue.enqueue(_));
	while(!queue.isEmpty)
	{
		val head = queue.dequeue;
		if(!visitedSorted.contains(head))
		{
			visitedSorted += head;
			val matchedRelations = fromTo.filter(_._1 == head).toList;
			if(matchedRelations.size > 1)
			{
				LOG.debug("FanOut > 1: " + exps);
			}
			matchedRelations.sortBy(x=>x._2).foreach(x=>queue.enqueue(x._2));
		}
	}
	LOG.trace("SORTED: "+visitedSorted.mkString(", ") + " of rule: " + exps);
//-------------------------------------------------------------------
  	//2)Get variables types
  	var varsPOS: Map[String, String] = Map();
  	var posIndexMap :Map[String, Int] = Map(("v"->3), ("n"->2), ("r"->1))
  	def addVarPos(varname:String, pos:String) = 
  	{
  		val posRenamed = if(pos == "a") "n" else pos /*replace "a" with "n"*/
  		val resPos = varsPOS.getOrElse(varname, posRenamed)
		val resPosIndex = posIndexMap(resPos)
		val posIndex = posIndexMap(posRenamed)			
		val newPos = (if(resPosIndex >= posIndex)
			resPos;
		else
			posRenamed);
		varsPOS = varsPOS + (varname ->newPos);
  	}
  	//Collect POS
  	exps.map
  	{
		case BoxerPred(discId, indices, variable, name, pos, sense) => {
			addVarPos(variable.name, pos); 
		}
		case BoxerRel(discId, indices, event, variable, name, sense) =>{
			addVarPos(event.name, "r");
			addVarPos(variable.name, "r");
		}
  	}
  	if(varsPOS.size != visitedSorted.size)
  		LOG.error ("size mismatch")
  	assert(varsPOS.size == visitedSorted.size)

  	val pattern = visitedSorted.map(varsPOS.get(_).get)
	//val sortedVarsPOS = varsPOS.toList.sortBy(_._1); //sort on the name. Names now are x1, x2, ... 
	//val pattern = sortedVarsPOS.map(_._2).mkString("");
	//rename variables again to PosFirst, PosIndex, PosLast 

	//rename vars in the predicates and relations
  	def getNewVariableName (oldName: BoxerVariable):BoxerVariable = 
  	{
		val varIndex = visitedSorted.indexOf(oldName.name);
		val varPos = pattern(varIndex);
		val firstSimilarPosIndex = pattern.indexOf(varPos);
		val lastSimilarPosIndex = pattern.lastIndexOf(varPos);
		val newVarSuffix = (
			if(firstSimilarPosIndex == varIndex)
			{
				"f"
			}
			else if(lastSimilarPosIndex == varIndex)
			{
				assert(firstSimilarPosIndex != lastSimilarPosIndex);
				"l"
			}
			else 
			{
				assert(firstSimilarPosIndex < varIndex);
				assert(lastSimilarPosIndex > varIndex);
				"m" + varIndex;
			}
		)
		return BoxerVariable(varPos + newVarSuffix);
  	}
	val expsNamesChanged: Set[BoxerExpression] = exps.map
  	{
		case BoxerPred(discId, indices, variable, name, pos, sense) => {
			BoxerPred(discId, indices, getNewVariableName(variable), name, pos, sense)
		}
		case BoxerRel(discId, indices, event, variable, name, sense) =>{
			BoxerRel(discId, indices, getNewVariableName(event), getNewVariableName(variable), name, sense)
		}
  	}
  	return (expsNamesChanged, pattern.mkString("")) //retrurn renamed expressions and pattern
  }
  
  def convertRulesToFOL(rules: List[String], text: BoxerExpression, hypothesis: BoxerExpression): List[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] =
  {
	val assumePredsList = text.getPredicates
	val goalPredsList = hypothesis.getPredicates
	val assumeRelsList = text.getRelations
	val goalRelsList = hypothesis.getRelations
	
	val (assumSentence, goalSentence) = if (Sts.opts.rulesMatchLemma)
		(Sts.textLemma, Sts.hypothesisLemma) else (Sts.text, Sts.hypothesis)

	//Mapping between 
	val (assumIndexMap, assumMetaRel) = buildIndexMap(assumePredsList, assumeRelsList, assumSentence);
	val (goalIndexMap, goalMetaRel) = buildIndexMap(goalPredsList, goalRelsList, goalSentence);

	val folRules = rules.flatMap { rule =>
		val Array(id, left, right, score) = rule.split("\t")
		val p1 = findPreds(left, assumePredsList, assumeRelsList, assumIndexMap, assumSentence);
        val exp1 = addMetaRels(p1, assumMetaRel);
        val p2 = findPreds(right, goalPredsList, goalRelsList, goalIndexMap, goalSentence);
        val exp2 = addMetaRels(p2, goalMetaRel)
        LOG.trace ("Paraphrase rule: (" + left + ") => " + exp1.map(simplePrintPred).mkString(", "));
        LOG.trace ("Paraphrase rule: (" + right+ ") => " + exp2.map(simplePrintPred).mkString(", "));
        LOG.trace ("DBG: ("+id+")(" + left + ", "+ right +") (" + exp1.map(simplePrintPred).mkString(", ") +") => ("+ exp2.map(simplePrintPred).mkString(", ") + ")");
        LOG.trace ("PTRN: ("+id+")(" + left + ", "+ right +") (" + exp1.map(simplerPrintPred).mkString(", ") +") => ("+ exp2.map(simplerPrintPred).mkString(", ") + ")");
        LOG.trace ("POS: ("+id+")(" + left + ", "+ right +") (" + printExpPattern(exp1) +") => ("+ printExpPattern(exp2) + ")");
        val pattern1 = sortVarsRenameVarsGetPattern(exp1);
        val pattern2 = sortVarsRenameVarsGetPattern(exp2);
        LOG.trace ("SrtRnm: " + pattern1._2 +"--" +  pattern1._1 + "-------" + exp1);
        LOG.trace ("SrtRnm: " + pattern2._2 +"--" + pattern2._1 + "-------" + exp2);
        val ruleTemplate = (
        		if (pattern2._2 > pattern1._2)
        			pattern2._2 +"--"+ pattern1._2
        		else
        			pattern1._2 +"--"+ pattern2._2
        )
        LOG.trace ("Template: " + ruleTemplate);
        
        var x=1;
        
        val leftTokens = left.split(" ").flatMap(token => Array(token, Lemmatize.lemmatizeWord(token)))
          .distinct

        // Find relating predicates for the lhs
        val matchAssumePreds = assumePredsList.filter { pred =>
          (pred.name == "topic" || leftTokens.contains(pred.name) || leftTokens.contains(pred.name + "s"))
        }
        val matchAssumePredVars = matchAssumePreds.map(pred => pred.variable.name)
        val matchAssumeRels = assumeRelsList.filter(rel =>
          (matchAssumePredVars.contains(rel.event.name) && matchAssumePredVars.contains(rel.variable.name))
            || ((matchAssumePredVars.contains(rel.event.name) || matchAssumePredVars.contains(rel.variable.name)
              || matchAssumePredVars.isEmpty)
              && leftTokens.contains(rel.name)))

        val rightTokens = right.split(" ").flatMap(token => Array(token, Lemmatize.lemmatizeWord(token)))
          .distinct

        // Find relating predicates for the rhs
        val matchGoalPreds = goalPredsList.filter { pred =>
          (pred.name == "topic" || rightTokens.contains(pred.name) || rightTokens.contains(pred.name + "s"))
        }
        val matchGoalPredVars = matchGoalPreds.map(pred => pred.variable.name)
        val matchGoalRels = goalRelsList.filter(rel =>
          (matchGoalPredVars.contains(rel.event.name) && matchGoalPredVars.contains(rel.variable.name))
            || ((matchGoalPredVars.contains(rel.event.name) || matchGoalPredVars.contains(rel.variable.name)
              || matchGoalPredVars.isEmpty)
              && rightTokens.contains(rel.name)))

        // Use this map to rename variables in the rhs
        var assumVarNameMap = Map[String, String]()
        var goalVarNameMap = Map[String, String]()

        var counter = 0;
        matchAssumePredVars.sortWith(_.compareTo(_) < 0).map(v => {
          assumVarNameMap += (v -> ("x" + counter.toString()));
          counter = counter + 1;
        })

        counter = 0;
        matchGoalPredVars.sortWith(_.compareTo(_) < 0).map(v => {
          goalVarNameMap += (v -> ("x" + counter.toString()));
          counter = counter + 1;
        })

        /*matchGoalPreds.map { pred =>()
				if(matchAssumePreds.size == 1 && matchGoalPreds.size == 1) 
					varNameMap += (pred.variable.name -> matchAssumePreds(0).variable.name) 
				else matchAssumePreds.foreach { assumePred =>
					val assumePredName = ("_" + assumePred.name + "_").replaceAll("_topic_", "_")
					val goalPredName = ("_" + pred.name + "_").replaceAll("_topic_", "_")
					if( (assumePred.toString.contains(",v,") && pred.toString.contains(",v,")) ||
						assumePredName.contains(goalPredName) || 
						goalPredName.contains(assumePredName)
					)
						varNameMap += (pred.variable.name -> assumePred.variable.name) 
				}
			}*/

        val changedMatchAssumPreds = matchAssumePreds.map { pred =>
          BoxerPred(pred.discId,
            pred.indices,
            BoxerVariable(assumVarNameMap.getOrElse(pred.variable.name, pred.variable.name)),
            pred.name,
            pred.pos,
            pred.sense)
        }
        val changedMatchAssumRels = matchAssumeRels.map { rel =>
          BoxerRel(rel.discId,
            rel.indices,
            BoxerVariable(assumVarNameMap.getOrElse(rel.event.name, rel.event.name)),
            BoxerVariable(assumVarNameMap.getOrElse(rel.variable.name, rel.variable.name)),
            rel.name,
            rel.sense)
        }
        ////////////////////////			

        val changedMatchGoalPreds = matchGoalPreds.map { pred =>
          BoxerPred(pred.discId,
            pred.indices,
            BoxerVariable(goalVarNameMap.getOrElse(pred.variable.name, pred.variable.name)),
            pred.name,
            pred.pos,
            pred.sense)
        }
        val changedMatchGoalRels = matchGoalRels.map { rel =>
          BoxerRel(rel.discId,
            rel.indices,
            BoxerVariable(goalVarNameMap.getOrElse(rel.event.name, rel.event.name)),
            BoxerVariable(goalVarNameMap.getOrElse(rel.variable.name, rel.variable.name)),
            rel.name,
            rel.sense)
        }

        val leftFOL = matchAssumePreds ++ matchAssumeRels
        val rightFOL = matchGoalPreds ++ matchGoalRels
        //val leftFOL = changedMatchAssumPreds ++ changedMatchAssumRels
        //val rightFOL = changedMatchGoalPreds ++ changedMatchGoalRels

        if (leftFOL.isEmpty || rightFOL.isEmpty)
          None
        else {

          val lhsDrs = BoxerDrs((matchAssumePredVars ++ matchGoalPredVars).map(v => (List() -> BoxerVariable(v))).toList, leftFOL.toList);
          val rhsDrs = BoxerDrs((matchAssumePredVars ++ matchGoalPredVars).map(v => (List() -> BoxerVariable(v))).toList, rightFOL.toList);
          
          List((lhsDrs, rhsDrs, score.toDouble, RuleType.Implication))
        }
      }
      return folRules;
    }
  
  private def buildIndexMap (preds: Seq[BoxerPred], rels: Seq[BoxerRel], lemma:String): (MultiMap[Int, BoxerExpression], Set[BoxerRel]) = 
  {
	  val indexMap = new HashMap[Int, Set[BoxerExpression]] with MultiMap[Int, BoxerExpression]
	  var metaRelations: Set[BoxerRel] = Set();
	  preds.foreach(pred=>{
	    if(pred.indices.size == 0)  //Ignore them. I do not need them in the inference rules: 
	      /*
		      1 event
		      6 person
		     27 thing
		     31 topic 
	      */
	      LOG.debug("Logical predicate with no matching token: " + pred)
	    pred.indices.foreach(idx => {
	      indexMap.addBinding(idx.wordIndex, pred)
	    })
	  })
	  rels.foreach(rel=>{
		if(rel.indices.size == 0)
		{
	      LOG.debug("Logical relation with no matching token: " + rel)
	      /*  //They are important parts of the inference rules: 
		  1 loc_rel
	     11 that
	     20 on
	     23 recipient
	     25 in
	    104 for
	    279 theme
	   2050 subset_of
	   2507 of
	   6839 patient
	  10742 agent
	      */
	      metaRelations = metaRelations + rel;  //add it to the set of meta relations
		}
	    rel.indices.foreach(idx => {
	      indexMap.addBinding(idx.wordIndex, rel)
	    })
	  })

	  val lemmaTokens = lemma.split(" ");
	  if (indexMap.keys.max >= lemmaTokens.length)
	    throw new RuntimeException("BoxerIndex larger than number of tokens in the sentence")
	  indexMap.values.foreach(m=>{
	    if(m.size > 1)
	      LOG.debug ("Map of size > 1: " + m);
	  })

	  for(i <-0 until lemmaTokens.size)
	  {
	    val token = lemmaTokens(i);
	    val boxerExprSet = indexMap.getOrElse(i, Set())
	    if(boxerExprSet.size == 0 )
	    {
	      LOG.debug("Token: (" + token + ") has no logical match")
	      /* List of tokens with no logical match 
			1 all 
			2 do 
			2 this 
			3 or 
			5 in 
			6 have 
			7 of 
			12 each 
			14 both 
			18 different 
			31 . 
			35 five 
			42 to 
			44 other 
			73 n't 
			75 four 
			86 another 
			131 who 
			137 that 
			144 three 
			183 which 
			216 one 
			380 not 
			387 some 
			399 , 
			590 an 
			621 there 
			658 no 
			740 two 
			2300 and 
			5216 the 
			11650 be 
			13875 a
			* 
			*/ 
	    }
	    else
	    {
	      var isPred = false;
	      var isRel = false;
	      boxerExprSet.foreach{
	        case BoxerPred(discId, indices, variable, name, pos, sense) =>
	          isPred = true;
	          if(isRel)
	            throw new RuntimeException("Pred and Rel at the same time");
	          if(!isLemmaExpnameMatch(token, name)) LOG.error ("Expecting token: (" + token+ ") but found pred named: (" + name + ")");

	        case BoxerRel(discId, indices, event, variable, name, sense) =>
	          isRel = true;
	          if(isPred)
	            throw new RuntimeException("Pred and Rel at the same time");
	          if(!isLemmaExpnameMatch(token, name)) LOG.error ("Expecting token: (" + token+ ") but found rel named: (" + name + ")");
	      }
	    }
	  }

	  return (indexMap, metaRelations); 
  }
  private def isLemmaExpnameMatch (lemma: String, exp: String) : Boolean = 
  {
	  if (lemma == exp)
	    return true;
	  if (lemma == Lemmatize.lemmatizeWord(exp))
	    return true;
	  val ignoredPairs = List(
		("likes","like"),
		("nothing","thing"),
		("where","location"),
		("you","person"),
		("her","she"),
		("themselve","group"),
		("the","in"),
		("nobody","person"),
		("tortilla","tortillum"),  //<<
		("himself","male"),
		("other","person"),
		("she","female"),
		("somebody","person"),
		("something","thing"),
		("without","with"),
		("a","for"),  //<<
		("many","quantity"),
		("another","thing"),
		("him","male"),
		("someone","person"),
		(",","rel"),
		("them","thing"),
		("their","thing"),
		("each","thing"),
		("'","of"),
		("her","female"),
		("its","thing"),
		("his","male"),
		("by","agent"),
		("it","thing"),
		("they","thing"));
	  if (ignoredPairs.contains((lemma, exp)))
	    return true;
	  
	  return false;
  }
  private def findPreds(phrase: String, preds: Seq[BoxerPred], rels: Seq[BoxerRel], indexMap: MultiMap[Int, BoxerExpression], sentence: String): List[Set[BoxerExpression]] = 
  {
	val sentenceTokens = sentence.split(" ").filterNot(Rules.tokensToRemoveFromRules.contains(_));
	val lemmaSplits = sentence.split(" ")
	val lemmaTokensIndex = lemmaSplits.indices.map(idx=> (idx, lemmaSplits(idx)) ).filterNot(pair=>Rules.tokensToRemoveFromRules.contains(pair._2));
	val lemmaIndices = lemmaTokensIndex.unzip._1
	val lemmaTokens = lemmaTokensIndex.unzip._2
	//println(sentenceTokens.mkString(" "))
	//println(lemmaTokens.mkString(" "))
	//require(sentenceTokens.length == lemmaTokens.length)
 
	val phraseTokens = if(Sts.opts.rulesMatchLemma)
			Tokenize(phrase.toLowerCase()).map(Lemmatize.lemmatizeWord).filterNot(Rules.tokensToRemoveFromRules.contains(_)).toArray
		else
			Tokenize(phrase.toLowerCase()).filterNot(Rules.tokensToRemoveFromRules.contains(_)).toArray

	if(!sentenceTokens.containsSlice(phraseTokens))
	  throw new RuntimeException("A matched rule "+phraseTokens.mkString(",")+" could not be found in the sentnece(" + lemmaTokens.mkString(", "));
	
	var sliceIndex:Int = sentenceTokens.indexOfSlice(phraseTokens)
	val lemmaIndicesSlice = lemmaIndices.slice(sliceIndex, sliceIndex + phraseTokens.length )

	val firstBoxerIndex = lemmaIndicesSlice.head
	val LastBoxerIndex = lemmaIndicesSlice.last

	//TODO: collect preds and rels from indexMap on indices on the range (firstBoxerIndex, LastBoxerIndex)
	LOG.trace("Rule part (" + phrase + ") is mapped to the sets: ")
	val predsAndRels = Range(firstBoxerIndex, LastBoxerIndex+1).map(index=>indexMap.getOrElse(index, Set())).flatMap(s=> {
		if(s.size > 1)
		  LOG.trace ("Set of size > 1: " + s);
		else 
		  LOG.trace (s);
		if (s.size >= 1)
		  Some(s);
		else 
		  None
	}).toList

	predsAndRels
  }
  private def addMetaRels(nonmetaPredRel: List[Set[BoxerExpression]], metaRel: Set[BoxerRel]): Set[BoxerExpression] = 
  {
	val sortedNonmeta = nonmetaPredRel.sortBy(s => s.size);
	var selectedVars:Set[String] = Set ();
	var visited:Set[Set[BoxerExpression]] = Set();
	var lastVisitedCount:Int = -1;
	var selectedExp:Set[BoxerExpression] = Set();
	var usedMetaRel:Set[BoxerRel] = Set();	
	while (visited.size < sortedNonmeta.size)
	{
		if (lastVisitedCount == visited.size)
		{
			//TODO: usually this is because a longer chain connecting the predicates
			//The problem is that if you allow longer chains, you will get additional crap
			LOG.error("Some predicates are not reachable: (" + nonmetaPredRel + ")"); 
			//throw new RuntimeException("Some predicates are not reachable: (" + nonmetaPredRel + ")");			
			return Set();
		}

		lastVisitedCount = visited.size;
		sortedNonmeta.foreach(set =>
		{
			assert (set.size>0, "Empty set");
			if (!visited.contains(set))
			{
				breakable {set.foreach(exp => 
				{
					val currentVars:Set[String] = exp match {
						case BoxerPred(discId, indices, variable, name, pos, sense) => Set((variable.name)); 
						case BoxerRel(discId, indices, event, variable, name, sense) =>Set (variable.name, event.name);
					}
					val directConnectingMetaEdges:Set[BoxerRel] = metaRel.flatMap(rel=>{
						if (!usedMetaRel.contains(rel)
							&&
							(//metaRelation is a connecting edge
								(selectedVars.contains(rel.event.name) && currentVars.contains(rel.variable.name)) ||
								(selectedVars.contains(rel.variable.name) && currentVars.contains(rel.event.name))
							)
						)
						{
							Some(rel)
						}
						else 
							None
					})

					//indirect connection using two relations and a shared event variable 
					val indirectConnectingMetaEdges:Set[BoxerRel] = metaRel.flatMap(rel1=>{ 
						metaRel.flatMap(rel2=>
						{
							if (!usedMetaRel.contains(rel1) && !usedMetaRel.contains(rel2) 
								&&
								rel1 != rel2
								&&
								(//metaRelation is a connecting edge
									//(selectedVars.contains(rel1.event.name) && rel1.variable.name == rel2.event.name && currentVars.contains(rel2.variable.name)) ||
									//(selectedVars.contains(rel1.event.name) && rel1.variable.name == rel2.variable.name && currentVars.contains(rel2.event.name)) ||
									(selectedVars.contains(rel1.variable.name) && rel1.event.name == rel2.event.name && currentVars.contains(rel2.variable.name)) //||
									//(selectedVars.contains(rel1.variable.name) && rel1.event.name == rel2.variable.name && currentVars.contains(rel2.event.name))
								)
							)
							{
								Set(rel1, rel2)
							}
							else 
								None
						})
					})
					None
					if( //set.size == 1 || /*a set with a single pred (wrong. It results in disconnected rules)*/
					selectedVars.size == 0 || /*no predicates picked before*/ 
					!(selectedVars & currentVars).isEmpty /*connected to some previous pred or rel*/ ||
					directConnectingMetaEdges.size != 0 /*There is a meta relation connecting this expression to the selected expression*/ || 
					indirectConnectingMetaEdges.size != 0 /*There are two meta relations connecting this expression to the selected expression indirectly*/)
					{
						usedMetaRel = usedMetaRel ++ directConnectingMetaEdges;
						visited = visited + set;
						selectedExp = selectedExp + exp;
						selectedExp = selectedExp ++ directConnectingMetaEdges

						if(selectedVars.size == 0 ||  !(selectedVars & currentVars).isEmpty || directConnectingMetaEdges.size != 0 )
						{
							None //No need to add indirect connecting meta edges 
						}
						else
						{
							//Add indirect connecting meta edges only if they are necessary 
							usedMetaRel = usedMetaRel ++ indirectConnectingMetaEdges;
							selectedExp = selectedExp ++ indirectConnectingMetaEdges
							LOG.trace("indirect edge added");
						}

						selectedVars = selectedVars ++ currentVars;						
						break;
					}
				})}
			}
		})
/*		if (!reachedAny)
		{	//search in meta relations
			metaRel.filter(rel=>{
				!usedMetaRel.contains(rel) && (selectedVars.contains(rel.variable.name)||selectedVars.contains(rel.event.name))  
			})
		}
		* 
		*/
	}
	
/*
val matchAssumePredVars = matchAssumePreds.map(pred => pred.variable.name)
val matchAssumeRels = assumeRelsList.filter(rel =>
  (matchAssumePredVars.contains(rel.event.name) && matchAssumePredVars.contains(rel.variable.name))
    || ((matchAssumePredVars.contains(rel.event.name) || matchAssumePredVars.contains(rel.variable.name)
      || matchAssumePredVars.isEmpty)
      && leftTokens.contains(rel.name)))
      * 
      */
	selectedExp;
  }
  
  private def changeExpDirection(e: BoxerExpression): BoxerExpression =
    {
      e match {
        case BoxerRel(discId, indices, event, variable, name, sense) => BoxerRel(discId match { case "h" => "t"; case "t" => "h"; }, indices, event, variable, name, sense);
        case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discId match { case "h" => "t"; case "t" => "h"; }, indices, variable, name, pos, sense);
        case _ => e.visitConstruct(changeExpDirection);
      }
    }

  def createWeightedExpression(leftFOL: BoxerDrs, rightFOL: BoxerDrs, score: Double, ruleType: RuleType.Value): List[WeightedExpression[BoxerExpression]] =
  {
    if(score < Sts.opts.weightThreshold )
      return List();
  
    List(createWeightedExpression(leftFOL, rightFOL, "h", score, ruleType)).flatten ++
    (
      if (Sts.opts.task == "sts")
        List(createWeightedExpression(rightFOL, leftFOL, "t", score, ruleType)).flatten
      else List())
  }

  private def createWeightedExpression(leftFOL: BoxerDrs, rightFOL: BoxerDrs, discId: String, score: Double, ruleType: RuleType.Value): Option[WeightedExpression[BoxerExpression]] =
    {
      val lhsSet = leftFOL.refs.map(_._2).toSet
      val rhsSet = rightFOL.refs.map(_._2).toSet
      val diff = rhsSet -- lhsSet; 
      if( ! diff.isEmpty )
      {
        //println ("Rule ignored: " + leftFOL + " => " + rightFOL )
        return None //RHS has variables that are not in the LHS
      }

      val changedLHS = changeExpDirection(leftFOL);
      var lhs = changedLHS.asInstanceOf[BoxerDrs]();      
      var rhs = rightFOL
      val allRefs = (lhs.refs ++ rhs.refs).toSet.toList
      if(ruleType  == RuleType.BackwardImplication )
      {
    	  rhs = BoxerDrs(allRefs, rhs.conds);
    	  lhs = BoxerDrs(List(), lhs.conds);
      }
      else
      {
    	  rhs = BoxerDrs(List(), rhs.conds);
    	  lhs = BoxerDrs(allRefs, lhs.conds);        
      }
      val unweightedRule = ruleType match {
        case RuleType.BackwardImplication => BoxerImp(discId, List(), rhs, lhs) 
        case RuleType.Implication => BoxerImp(discId, List(), lhs, rhs)
        case RuleType.DoubleImplication => BoxerEqv(discId, List(), lhs, rhs)
        case RuleType.Opposite => BoxerEqv(discId, List(), lhs, BoxerNot(discId, List(), rhs))
      }
      return Some(SoftWeightedExpression(unweightedRule, score))
    }
}
