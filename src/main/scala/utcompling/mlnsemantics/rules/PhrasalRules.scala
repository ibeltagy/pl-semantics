package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import utcompling.mlnsemantics.inference.support.SimplePhrase
import utcompling.mlnsemantics.inference.support.RelationalPhrase
import utcompling.mlnsemantics.inference.support.SimplePhrase
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable


object PhrasalRules {

  private val LOG = LogFactory.getLog(classOf[Rules]) 


  def findSimplePhrases(preds: Seq[BoxerPred]): Seq[SimplePhrase] =
  {
	  var varPhraseMap:Map[String, List[BoxerPred]] = Map();
	  for (pred <- preds)
	  {
		  var variableName = pred.variable.name;
		  var listPreds = varPhraseMap.getOrElse(variableName, List());
		  listPreds = listPreds :+ pred;
		  varPhraseMap = varPhraseMap + (variableName -> listPreds)
	  }
	  return varPhraseMap.map(p =>{
	    var sortedPreds = p._2.sortBy(x=>x.pos);
	    sortedPreds  = sortedPreds.reverse;
	    assert(sortedPreds.head.pos == "n" ||sortedPreds.head.pos == "v")
	    sortedPreds.tail.foreach(x=>assert(x.pos=="a"));
	    SimplePhrase(sortedPreds.head, sortedPreds.tail);
	  }).toSeq
  }
  
  def findRelationalPhrases(rels: Seq[BoxerRel], simplePhrases: Seq[SimplePhrase]): Seq[RelationalPhrase] =
  {
	  var varSimplePhraseMap:Map[BoxerVariable, SimplePhrase] = simplePhrases.map(ph => (ph.getVariable->ph)).toMap

	  for (pred <- preds)
	  {
		  var variableName = pred.variable.name;
		  var listPreds = varPhraseMap.getOrElse(variableName, List());
		  listPreds = listPreds :+ pred;
		  varPhraseMap = varPhraseMap + (variableName -> listPreds)
	  }
	  return varPhraseMap.map(p =>{
	    var sortedPreds = p._2.sortBy(x=>x.pos);
	    sortedPreds  = sortedPreds.reverse;
	    assert(sortedPreds.head.pos == "n" ||sortedPreds.head.pos == "v")
	    sortedPreds.tail.foreach(x=>assert(x.pos=="a"));
	    SimplePhrase(sortedPreds.head, sortedPreds.tail);
	  }).toSeq
  }  
}