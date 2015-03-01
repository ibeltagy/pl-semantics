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
		val Array(id, ruleLhs, ruleRhs, score, direction) = rule.split("\t")
		val (left, right) = if (direction == ""+RuleType.Implication) (ruleLhs, ruleRhs);
								else (ruleRhs, ruleLhs);
		//println (rule + " ## "  + left + " ## " + right + "##" +direction + "#")
		var inclusion : Boolean = false; // the "parking lot -> parking" case 
		if(left.contains(right) && Sts.hypothesis.contains(left) || right.contains(left) && Sts.text.contains(right))
		{
			LOG.trace("Rule inclusion: " + left + "->" +right);
			None
		}
		else
		{
			val p1 = findPreds(left, assumePredsList, assumeRelsList, assumIndexMap, assumSentence);
	        val exp1 = addMetaRels(p1, assumMetaRel);
	        val p2 = findPreds(right, goalPredsList, goalRelsList, goalIndexMap, goalSentence);
	        val exp2 = addMetaRels(p2, goalMetaRel)
	        LOG.trace ("Paraphrase rule: (" + left + ") => " + exp1.map(simplePrintPred).mkString(", "));
	        LOG.trace ("Paraphrase rule: (" + right+ ") => " + exp2.map(simplePrintPred).mkString(", "));
	        LOG.trace ("DBG: ("+id+")(" + left + ", "+ right +") (" + exp1.map(simplePrintPred).mkString(", ") +") => ("+ exp2.map(simplePrintPred).mkString(", ") + ")");
	        LOG.trace ("PTRN: ("+id+")(" + left + ", "+ right +") (" + exp1.map(simplerPrintPred).mkString(", ") +") => ("+ exp2.map(simplerPrintPred).mkString(", ") + ")");
	        LOG.trace ("POS: ("+id+")(" + left + ", "+ right +") (" + printExpPattern(exp1) +") => ("+ printExpPattern(exp2) + ")");
	        var pattern1 = sortVarsRenameVarsGetPattern(exp1);
	        var pattern2 = sortVarsRenameVarsGetPattern(exp2);
	        LOG.trace ("SrtRnm: " + pattern1._2 +"--" +  pattern1._1 + "-------" + exp1);
	        LOG.trace ("SrtRnm: " + pattern2._2 +"--" + pattern2._1 + "-------" + exp2);
	        val ruleTemplate = (
	        		if (pattern2._2 > pattern1._2)
	        			pattern2._2 +"--"+ pattern1._2
	        		else
	        			pattern1._2 +"--"+ pattern2._2
	        )
	        LOG.trace ("Template: " + ruleTemplate);
			  if (pattern1._2 == "" || pattern2._2 == "")
					None
			  else  //both patterns are not empty
			  {
	        		val knownPatterns = List(
	("n--n" /*15496*/, "M"),
	("nn--n" /*786*/, "M"),
	("nn--nn" /*27*/, "M"),
	("nr--nr" /*258*/, "M"), ("rn--nr" /*47*/, "I"), ("rn--rn" /*110*/, "M"),
	("rnr--rnr" /*8*/, "M"), ("rrn--rrn" /*2*/, "M"),
	("vn--vn" /*3*/, "M"),
	("r--nr" /*18*/, "M"), ("rn--r" /*34*/, "M"),
	("r--r" /*32*/, "M"),
	("rr--rnr" /*804*/, "M"), ("rrn--rr" /*34*/, "I"),
	("rr--rr" /*885*/, "M"),
	("rrr--rr" /*12*/, "M"),
	("vr--vnr" /*10*/, "M"),
	("vr--vr" /*147*/, "M"),
	("vn--v" /*75*/, "M"),
	("vr--v" /*934*/, "P"),
	("v--v" /*1684*/, "M"),
	("rnn--n" /*15*/, "I"),
	("nr--n" /*344*/, "I"), ("rn--n" /*378*/, "I"),
	("nr--nn" /*152*/, "I"), ("rn--nn" /*147*/, "I"),
	("rnr--n" /*8*/, "I"), ("rrn--n" /*1*/, "I"),
	("rnr--nn" /*1*/, "I"),
	("vnr--nr" /*1*/, "I"),
	("vn--n" /*73*/, "I"),
	("vn--rn" /*3*/, "I"),
	("r--n" /*73*/, "I"),
	("rnr--r" /*9*/, "I"),
	("rr--n" /*417*/, "I"),
	("rr--nr" /*325*/, "I"), ("rr--rn" /*1281*/, "I"),
	("rr--r" /*230*/, "I"),
	("rrr--nr" /*5*/, "I"),
	("vr--n" /*22*/, "I"),
	("vr--nr" /*48*/, "I"), ("vr--rn" /*19*/, "I"),
	("vr--rnr" /*23*/, "I"),
	("vr--rr" /*106*/, "I"),
	("v--n" /*710*/, "I"),
	("v--nn" /*5*/, "I"),
	("v--nr" /*29*/, "I"), ("v--rn" /*22*/, "I"),
	("v--rnr" /*11*/, "I"),
	("v--r" /*57*/, "I"),
	("v--rr" /*769*/, "I"),
	("rn--nrr" /*2*/, "I"), ("rnr--nr" /*13*/, "I"), ("rrn--rn" /*3*/, "I"),
	("rnn--nn" /*4*/, "I"),
	("rrrn--n" /*4*/, "I")
					).toMap;
	
					if (!knownPatterns.contains(ruleTemplate))
						LOG.error("Rule template not exist: " + ruleTemplate);
					
					val matchOrIgnore = knownPatterns.getOrElse(ruleTemplate, "I")
					if(matchOrIgnore == "P") // add patient or remove agent 
					{
						assert(pattern2._2 == "v" && pattern1._2 == "vr" || pattern2._2 == "vr" && pattern1._2 == "v")
						def removeAgentAddPatient (arg1:(Set[BoxerExpression], String), arg2:(Set[BoxerExpression], String)): 
						((Set[BoxerExpression], String), (Set[BoxerExpression], String)) =
						{
							var newArg1 = arg1;
							var newArg2 = arg2;
							val filteredExps = arg1._1.filter(
							{
								case BoxerRel(discId, indices, event, variable, "agent", sense) => false 
								case _  => true
							})
							if (filteredExps.size < arg1._1.size)
							{
								assert(filteredExps.size + 1 ==  arg1._1.size);
								newArg1 = (filteredExps, "v");
							}
							else
							{
								val patient = BoxerRel(arg2._1.head.getPredicates.head.discId, List(), BoxerVariable("vf"), BoxerVariable("rf"), "patient", 0);
								newArg2 = (arg2._1 + patient, "vr");
							}
							(newArg1, newArg2)
						}
						if (pattern1._2 == "vr")
						{
							val res = removeAgentAddPatient(pattern1, pattern2);
							val existingPatients = hypothesis.getRelations.filter(rel => rel.name == "patient");
							if (res._2._2 == "vr" /* a patient was added */ && existingPatients.size != 0 /*there are patients*/ 
								|| res._2._2 == "v" /* no patient added */)
							{
								//apply changes
								pattern1 = res._1  
								pattern2 = res._2
							}
							//else keep it vr-v
						}
						else if (pattern2._2 == "vr")
						{
							val res = removeAgentAddPatient(pattern2, pattern1);
							val existingPatients = text.getRelations.filter(rel => rel.name == "patient");
							if (res._1._2 == "vr" /* a patient was added */ && existingPatients.size != 0 /*there are patients*/ 
								|| res._1._2 == "v" /* no patient added */)
							{
								//apply changes
								pattern2 = res._1
								pattern1 = res._2
							}
							//else keep it vr-v
						}
						else
							throw new RuntimeException("Not Reachable");
					}
					if (matchOrIgnore == "P" || matchOrIgnore == "M")
					{
						//val lhsVars:List[String] = pattern1._1.toList.flatMap(exp => getVariables(exp).map(v=>v.name));
						//val rhsVars:List[String] = pattern2._1.toList.flatMap (exp => getVariables(exp).map(v=>v.name));
						val lhsDrs = boxerAtomsToBoxerDrs(pattern1._1.toSet) //BoxerDrs(lhsVars.map(v => (List[BoxerIndex]() -> BoxerVariable(v))), pattern1._1.toList);
						val rhsDrs = boxerAtomsToBoxerDrs(pattern2._1.toSet)//BoxerDrs(rhsVars.map(v => (List[BoxerIndex]() -> BoxerVariable(v))), pattern2._1.toList);
						LOG.trace ("ParaphraseRule added: " + lhsDrs + " => " + rhsDrs);
						var s = score;
						if(s ==  "inf") s = "Infinity";
						else if (s == "-inf") s = "-Infinity";

						val (lhsDrsWithDirection, rhsDrsWithDirection) = if (direction == ""+RuleType.Implication)
																								(lhsDrs, rhsDrs)
																						else (rhsDrs, lhsDrs)
						if (s.toDouble > 0)
							List((lhsDrsWithDirection, rhsDrsWithDirection, s.toDouble, RuleType.Implication))
						else 
							List((lhsDrsWithDirection, rhsDrsWithDirection, -s.toDouble, RuleType.Opposite))
					}
					else
						None
			}
		}
      }
      return folRules;
    }
	def getVariables(exp:BoxerExpression):List[BoxerVariable] = 
	{
		return exp match 
		{
			 case BoxerPred(discId, indices, variable, name, pos, sense) => List(variable);
			 case BoxerRel(discId, indices, event, variable, name, sense) => List(event, variable)
		}
	}
  def boxerAtomsToBoxerDrs (atoms: scala.collection.immutable.Set[BoxerExpression]): BoxerDrs = 
  {
  	val vars :List[String] = atoms.toList.flatMap(exp => getVariables(exp).map(v=>v.name));
  	return BoxerDrs(vars.map(v => (List[BoxerIndex]() -> BoxerVariable(v))), atoms.toList);
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
	            throw new RuntimeException(name + " is a Pred and Rel at the same time");
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
  
  var discIdToUse:String = "";
  private def changeExpDirection(e: BoxerExpression): BoxerExpression =
    {
      e match {
        case BoxerRel(discId, indices, event, variable, name, sense) => BoxerRel(discIdToUse, indices, event, variable, name, sense);
        case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discIdToUse, indices, variable, name, pos, sense);
        case _ => e.visitConstruct(changeExpDirection);
      }
    }

  def renameVariablesAddTypes(drs: BoxerDrs, declarations: Map[String, Seq[String]]) : BoxerDrs = 
  {
  	var drsNewRefs : Set[BoxerVariable] = Set(); 
  	val drsNewConds = drs.conds.indices.map { index => 
  		drs.conds(index) match 
		{
			case BoxerPred(discId, indices, variable, name, pos, sense) => 
			{
				var newVarName = BoxerVariable(declarations(name).head + "_" + variable.name);
				drsNewRefs = drsNewRefs + newVarName; 
				BoxerPred(discId, indices, newVarName, name, pos, sense)	
			} 
			case BoxerRel(discId, indices, event, variable, name, sense) =>
			{
				var newVar1Name = BoxerVariable(declarations(name).head + "_" + event.name);
				var newVar2Name = BoxerVariable(declarations(name).last + "_" + variable.name);
				drsNewRefs = drsNewRefs ++ Set(newVar1Name, newVar2Name); 
				BoxerRel(discId, indices, newVar1Name, newVar2Name, name, sense)	
			}
			
			//case BoxerNamed(discId, indices, variable, name, typ, sense) => name
			//case BoxerCard(discId, indices, variable, num, typ) => "card_" + num
			//case BoxerTimex(discId, indices, variable, timeExp) => "time"
		}
  	}
  	assert(drsNewRefs.size == drs.refs.toSet.size)

	BoxerDrs(drsNewRefs.map((List[BoxerIndex]() -> _)).toList, drsNewConds.toList)
  }
  def createWeightedExpression(leftFOL: BoxerDrs, rightFOL: BoxerDrs, score: Double, ruleType: RuleType.Value, declarations: Map[String, Seq[String]]): List[WeightedExpression[BoxerExpression]] =
  {
    if(score < Sts.opts.weightThreshold )
      return List();
    val varRenamedLeftFol = renameVariablesAddTypes(leftFOL, declarations)
    val varRenamedRightFol = renameVariablesAddTypes(rightFOL, declarations)
    List(createWeightedExpression(varRenamedLeftFol, varRenamedRightFol, "h", score, ruleType)).flatten ++
    (
      if (Sts.opts.task == "sts")
        List(createWeightedExpression(varRenamedRightFol, varRenamedLeftFol, "t", score, ruleType)).flatten
      else List())
  }

  private def createWeightedExpression(leftFOL: BoxerDrs, rightFOL: BoxerDrs, discId: String, score: Double, ruleType: RuleType.Value): Option[WeightedExpression[BoxerExpression]] =
    {
      val lhsSet = leftFOL.refs.map(_._2).toSet
      val rhsSet = rightFOL.refs.map(_._2).toSet
      val diff = rhsSet -- lhsSet; 
      if( ! diff.isEmpty ) 
      {
      	//TODO: add existential quantifier, but ignore the rule for now.
        LOG.trace("Rule ignored: " + leftFOL + " => " + rightFOL )
        return None //RHS has variables that are not in the LHS
      }

		discIdToUse = discId
      val changedLHS = changeExpDirection(leftFOL);
      var lhs = changedLHS.asInstanceOf[BoxerDrs]();      
      var rhs = changeExpDirection(rightFOL).asInstanceOf[BoxerDrs]();
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
