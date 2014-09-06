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
  
  val tokensToRemoveFromRules = List("a", "an", "the")

  /**
   * Convert paraphrase rules in text format to FOL.
   * Rules have the format: <id> TAB <text_phrase> TAB <hypo_phrase> TAB <sim_score>
   */
  def convertRulesToFOL(rules: List[String], text: BoxerExpression, hypothesis: BoxerExpression): List[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] =
    {
      val assumePredsList = text.getPredicates
      val goalPredsList = hypothesis.getPredicates
      val assumeRelsList = text.getRelations
      val goalRelsList = hypothesis.getRelations

      val assumIndexMap = buildIndexMap(assumePredsList, assumeRelsList, Sts.textLemma);
      val goalIndexMap = buildIndexMap(goalPredsList, goalRelsList, Sts.hypothesisLemma);

      val folRules = rules.flatMap { rule =>
        val Array(id, left, right, score) = rule.split("\t")

        val p1 = findPreds(left, assumePredsList, assumeRelsList, assumIndexMap, Sts.text, Sts.textLemma);
        val p2 = findPreds(right, goalPredsList, goalRelsList, goalIndexMap, Sts.hypothesis, Sts.hypothesisLemma);
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
  
  private def buildIndexMap (preds: Seq[BoxerPred], rels: Seq[BoxerRel], lemma:String): MultiMap[Int, BoxerExpression] = 
  {
	  val indexMap = new HashMap[Int, Set[BoxerExpression]] with MultiMap[Int, BoxerExpression]
	  preds.foreach(pred=>{
	    pred.indices.foreach(idx => {
	      indexMap.addBinding(idx.wordIndex, pred)
	    })
	  })
	  rels.foreach(rel=>{
	    rel.indices.foreach(idx => {
	      indexMap.addBinding(idx.wordIndex, rel)
	    })
	  })

	  val lemmaTokens = lemma.split(" ");
	  if (indexMap.keys.max >= lemmaTokens.length)
	    throw new RuntimeException("BoxerIndex larger than number of tokens in the sentence")
	  indexMap.values.foreach(m=>{
	    if(m.size > 1)
	      println ("Map of size > 1: " + m);
	  })

	  for(i <-0 until lemmaTokens.size)
	  {
	    val token = lemmaTokens(i);
	    val boxerExprSet = indexMap.getOrElse(i, Set())
	    if(boxerExprSet.size == 0 )
	    {
	      println("Token: (" + token + ") has no logical match")
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
	          if(!isLemmaExpnameMatch(token, name)) println ("Expecting token: (" + token+ ") but found pred named: (" + name + ")");

	        case BoxerRel(discId, indices, event, variable, name, sense) =>
	          isRel = true;
	          if(isPred)
	            throw new RuntimeException("Pred and Rel at the same time");
	          if(!isLemmaExpnameMatch(token, name)) println ("Expecting token: (" + token+ ") but found rel named: (" + name + ")");
	      }
	    }
	  }

	  return indexMap; 
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
  private def findPreds(phrase: String, preds: Seq[BoxerPred], rels: Seq[BoxerRel], indexMap: MultiMap[Int, BoxerExpression], sentence: String , lemma: String): List[BoxerPred] = 
  {
	val sentenceTokens = sentence.split(" ").filterNot(Rules.tokensToRemoveFromRules.contains(_));
	val lemmaTokens = lemma.split(" ").filterNot(Rules.tokensToRemoveFromRules.contains(_));
	//println(sentenceTokens.mkString(" "))
	//println(lemmaTokens.mkString(" "))
	require(sentenceTokens.length == lemmaTokens.length)
	val phraseTokens = Tokenize(phrase.toLowerCase()).map(Lemmatize.lemmatizeWord).filterNot(Rules.tokensToRemoveFromRules.contains(_)).toArray

	if(!lemmaTokens.containsSlice(phraseTokens))
	  throw new RuntimeException("A matched rule could not be found in the sentnece");
	
	var sliceIndex:Int = lemmaTokens.indexOfSlice(phraseTokens)
	//println(lemmaTokens.slice(sliceIndex, sliceIndex + phraseTokens.length).mkString(" # "));
//	val firstBoxerIndex = sliceIndex; //zero based indexing
//	val LastBoxerIndex = sliceIndex + phraseTokens.length - 1;

	//TODO: collect preds and rels from indexMap on indices on the range (firstBoxerIndex, LastBoxerIndex)
	println("Rule part (" + phrase + ") is mapped to the sets: ")
	indexMap.slice(sliceIndex, sliceIndex + phraseTokens.length).foreach(s=> {
		println (s);
	})
/*    
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
              * 
              */
    List();
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
