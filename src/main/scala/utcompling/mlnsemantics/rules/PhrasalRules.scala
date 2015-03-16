package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable
import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerIndex


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
	    val metaPredsNouns = List("thing", "female", "male", "person", "quantity"/*Pair 566*/);
	    var sortedPreds = p._2.sortBy(x=> {
	      if (x.pos == "n" && (metaPredsNouns.contains(x.name))) 
	        1
	      else if (x.pos == "n" || x.pos == "v" )
	        2
	      else if (x.pos == "a" || x.pos == "r" )
	        1	        
	      else throw new RuntimeException("Unknown POS: " + x.pos + " in predicate: " + x)	
	    });
	    sortedPreds  = sortedPreds.reverse;
	    //assert(sortedPreds.head.pos == "n" ||sortedPreds.head.pos == "v" || sortedPreds.head.pos == "a"/*TODO: Pair 85*/ ||sortedPreds.head.pos == "r"/*TODO: Pair 237*/ )
	    //sortedPreds.tail.foreach(x=>{println(sortedPreds);assert(x.pos=="a" /*adjectives*/ || x.pos=="r" /*connected to verbs*/ ||
	    //															(x.pos == "n" && metaPredsNouns.contains(x.name)))});
	    SimplePhrase(sortedPreds.head, sortedPreds.tail);
	  }).toSeq
  }
  
  def findRelationalPhrases(rels: Seq[BoxerRel], simplePhrases: Seq[SimplePhrase]): Seq[RelationalPhrase] =
  {
	  var varSimplePhraseMap:Map[String, SimplePhrase] = simplePhrases.map(ph => (ph.getVariable.name->ph)).toMap

	  val relPhrases = rels.flatMap(rel=>
	  {
		  var variableName = rel.variable.name;
		  var matchedSimplePhrase:SimplePhrase = varSimplePhraseMap.getOrElse(variableName, null);
		  if(matchedSimplePhrase == null)  //The ugly "together" and "each other".
		    None							//TODO: make sure it is only in case of "together" and "each other" 
		  else Some(RelationalPhrase(rel, matchedSimplePhrase))
	  })
	  return relPhrases;
  }
  
  def findNounPhrases(simplePhrases: Seq[SimplePhrase]): Seq[NounPhrase] =
  {	  
      simplePhrases.flatMap(ph=>{
	    if(ph.getPos == "n")
	      Some(NounPhrase(ph))
	    else if(ph.getPos == "v")
	      None
	    else if(ph.getPos == "a")
	      None  //TODO: Pair 85	      
	    else if(ph.getPos == "r")
	      None  //TODO: Pair 237	      
	    else
	      throw new RuntimeException("Unsupported SimplePhrase of POS: " + ph.getPos)
	  })
  }
  
  def findPrepPhrase(simplePhrases: Seq[SimplePhrase], relationalPhrases: Seq[RelationalPhrase]): Seq[PrepPhrase] =
  {
	var varSimplePhraseMap:Map[String, SimplePhrase] = simplePhrases.map(ph => (ph.getVariable.name->ph)).toMap
	relationalPhrases.flatMap(relPh=>{
	  if (relPh.isPreposition)
	  {
		  val simplePh = varSimplePhraseMap.getOrElse(relPh.getHeadVariable.name, null)
		  if(simplePh != null && simplePh.getPos != "v") //because prepositions connected with verbs are part of VerbPhrase
			  											//not PrepPhrase
		  {
			  Some(PrepPhrase(simplePh, List(relPh)))//Skip the chaining for now 
		  }
		  else None
	  }
	  else None
	})  
  }  
  
  def findVerbPhrase(simplePhrases: Seq[SimplePhrase], relationalPhrases: Seq[RelationalPhrase]): Seq[VerbPhrase] =
  {
	val varRelationalPhraseMap = new HashMap[String, Set[RelationalPhrase]] with MultiMap[String, RelationalPhrase]
    relationalPhrases.foreach(ph => varRelationalPhraseMap.addBinding(ph.getHeadVariable.name, ph))
	
    simplePhrases.flatMap(simplePh=>{
	  if (simplePh.getPos == "v") //only verbs 
	  {
		  val relationalPhs = varRelationalPhraseMap.getOrElse(simplePh.getVariable.name, null)
		  if(relationalPhs != null)
		  {
		    var agents = relationalPhs.filter(_.head.name == "agent"); 
		    var patientsAndPrep = relationalPhs.filter(relPh => {relPh.head.name == "patient" || relPh.isPreposition});
		    var noneOfAbove = relationalPhs.filterNot(relPh => 
		      	{relPh.head.name == "patient" || relPh.isPreposition || relPh.head.name == "agent" ||//handled above 
		      	relPh.head.name == "theme" || relPh.head.name == "recipient"});//TODO: Pair 126
		    if(agents.size != 1)
		    {
		      None//Verb without a "direct" subject, for example "together" and "each other"
		      //TODO: make sure this case only happens in "together" and "each other"
		      //throw new RuntimeException("Unexpected number of agents of a very: " + agents.size + " agents");
		    }
		    else
		    {
			    if(noneOfAbove.size != 0)
			      throw new RuntimeException("Unexpected relations connected to a verb: " + noneOfAbove); 
			    if (patientsAndPrep.size == 0)
			    	List(VerbPhrase(simplePh, agents.toList, List()))//Skip the chaining for now
			    else
			    	patientsAndPrep.map(obj => VerbPhrase(simplePh, agents.toList, List(obj))).toList
		    }
		  }
		  else None
	  }
	  else None
	})
  }
  
  def ruleSideToString (exps:List[BoxerExpression], sentence: String, simple: Boolean): String = 
  {
  	  val sentenceTokens = sentence.split(" ");
  	  def indicesToOneIndex ( l:List[BoxerIndex]) : List[BoxerIndex] = 
  	  {
  	  	if (l.size <= 1)
  	  		return l;
  	  	return List(l.head);
  	  }
	  val namesList = exps.flatMap(exp => {
	  		exp match
	  		{
	  			case BoxerPred(discId, indices, variable, name, pos, sense) => 
	  			{
	  				val index = Rules.indicesToIndex(indicesToOneIndex(indices));
	  				if (index == 0)
	  					None
	  				else
	  				{
	  					if (simple)
	  						Some((index, sentenceTokens(index-1)))
	  					else 
	  						Some((index, sentenceTokens(index-1) + "-" + pos + "-" + index))
	  				}
	  			}
	  			case BoxerRel(discId, indices, event, variable, name, sense) => 
	  			{
	  				val index = Rules.indicesToIndex(indicesToOneIndex(indices));
	  				if (index == 0)
	  					None
	  				else
	  				{
	  					if (simple)
	  						Some((index, sentenceTokens(index-1)))
	  					else
	  						Some((index, sentenceTokens(index-1) + "-r-" + index))
	  				}
	  			}
	  		}
	  })
	  val filteredSorted = namesList.toSet.toList.filter(_._1 != 0).sortBy(_._1);
	  filteredSorted.map(_._2).mkString(" ")
  }
    
  def isCompatible(lhs:Phrase, rhs:Phrase):Boolean = 
  {
	if (lhs.isInstanceOf[NounPhrase] &&  rhs.isInstanceOf[NounPhrase])
		return true;
	if (lhs.isInstanceOf[PrepPhrase] &&  rhs.isInstanceOf[PrepPhrase])
		return true;
	if (lhs.isInstanceOf[NounPhrase] &&  rhs.isInstanceOf[PrepPhrase])
		return true;
	if (lhs.isInstanceOf[PrepPhrase] &&  rhs.isInstanceOf[NounPhrase])
		return true;
	if (lhs.isInstanceOf[VerbPhrase] &&  rhs.isInstanceOf[VerbPhrase])
		return true;
	return false
  }
}