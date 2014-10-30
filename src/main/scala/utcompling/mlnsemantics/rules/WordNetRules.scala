package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.run.Sts
import scala.Array.canBuildFrom
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.{BowVector, BowVectorSpace}
import utcompling.mlnsemantics.inference.RuleWeighter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerIndex
import utcompling.mlnsemantics.wordnet.WordnetImpl

class WordNetRules extends Rules {

  private val LOG = LogFactory.getLog(classOf[WordNetRules])


  def getRules(text: BoxerExpression, hypothesis: BoxerExpression): List[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] =
    {
	  if(!Sts.opts.wordnet)
	    return List();

	  val txtPreds = text.getPredicates();
	  val hypPreds = hypothesis.getPredicates();

	  var rules = List[(BoxerDrs, BoxerDrs, Double, RuleType.Value)]() ; 
	  
	  for (txtPred <- txtPreds )
      {
		  val synonyms = WordNetRules.wordnet.getSynonyms(txtPred.name, txtPred.pos);
		  val hypernyms = WordNetRules.wordnet.getHypernyms(txtPred.name, txtPred.pos);
		  val hyponyms = WordNetRules.wordnet.getHyponyms(txtPred.name, txtPred.pos);
		  val antonyms = WordNetRules.wordnet.getAntonyms(txtPred.name, txtPred.pos);
		  //println (txtPred.name + " -->" + antonyms )

          for (hypPred <- hypPreds )
          {
        	 if (hypPred.pos == txtPred.pos && hypPred.name != txtPred.name)
        	 {
        		 if (synonyms.contains(hypPred.name))
        		 {								//DoubleImplication
        		   rules = rules :+ createRule (txtPred, hypPred, RuleType.Implication); 
        		   rules = rules :+ createRule (txtPred, hypPred, RuleType.BackwardImplication);
        		 }
        		 if (hypernyms.contains(hypPred.name))
        			  rules = rules :+ createRule (txtPred, hypPred, RuleType.Implication);
        		 if (hyponyms.contains(hypPred.name))   //backward implication 
        		 	  rules = rules :+ createRule (txtPred, hypPred, RuleType.BackwardImplication); //backward implication 
        		 if (antonyms.contains(hypPred.name) && Sts.opts.softLogicTool != "psl" /*PSL does not like this rule*/)
        			  rules = rules :+ createRule (txtPred, hypPred, RuleType.Opposite);
        	 }
          }
       } 
	   return rules;
    }
  	
  	def createRule(lhs: BoxerPred, rhs:BoxerPred, ruleType: RuleType.Value):(BoxerDrs, BoxerDrs, Double, RuleType.Value) = 
  	{
        val lhsDrs = BoxerDrs(List(List() -> BoxerVariable("x0")), List(
						BoxerPred(lhs.discId, lhs.indices, BoxerVariable("x0"), lhs.name, lhs.pos, lhs.sense)
						));
        val rhsDrs = BoxerDrs(List(List() -> BoxerVariable("x0")), List(
        				BoxerPred(rhs.discId, rhs.indices, BoxerVariable("x0"), rhs.name, rhs.pos, rhs.sense)
        				));
        (lhsDrs, rhsDrs, Double.PositiveInfinity, ruleType)
  	}
}

object WordNetRules {
  val wordnet = new WordnetImpl();
}
