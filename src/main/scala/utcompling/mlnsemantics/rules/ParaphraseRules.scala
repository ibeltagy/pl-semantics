package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.run.Sts
import scala.Array.canBuildFrom
import utcompling.mlnsemantics.datagen.Tokenize

class ParaphraseRules extends Rules{
  private val LOG = LogFactory.getLog(classOf[ParaphraseRules])

  // Search phrases in Text-Hypothesis pair

  def getRules(): List[String] = {

    val returnedRules = Sts.luceneParaphrases.query(Sts.text + " " + Sts.textLemma, Sts.hypothesis + " " + Sts.hypothesisLemma)

    val filterStart = System.nanoTime
    val paraphraseRules = returnedRules
      .filter { rule =>
        val Array(id, ruleLhs, ruleRhs, score) = rule.split("\t")
        //Some datasets use lemmas, other use non-lemmas, so, search in the sentence and the lemmatized sentence
        (process(Sts.text) +  process(Sts.textLemma)).contains(process(Tokenize.separateTokens(ruleLhs))) &&
        (process(Sts.hypothesis) +  process(Sts.hypothesisLemma)).contains(process(Tokenize.separateTokens(ruleRhs)))
        
      }.toList

    val filterEnd = System.nanoTime
    LOG.debug("Filtering time: " + (filterEnd - filterStart) / 1e9 + " s")
    LOG.trace("Paraphrase rules: ");
    paraphraseRules.foreach(rule => LOG.trace(rule))
    paraphraseRules
  }
  
  //process to match Barent rules. 
  def process(s:String):String = {
    val sLowerCase = s;
    val tokens = sLowerCase.split(" "); //It is enough to use "split" because the string is already tokenized
    val filteredTokens = tokens.filterNot(Rules.tokensToRemoveFromRules.contains(_))
    //the # are to avoid matching part of a token
    val stringToCompare = "#" + filteredTokens.mkString("#")+"#";
    return stringToCompare; 
  }
}

object ParaphraseRules {
  
}
