package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.run.Sts
import scala.Array.canBuildFrom
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.mlnsemantics.datagen.Lemmatize

class ParaphraseRules extends Rules{
  private val LOG = LogFactory.getLog(classOf[ParaphraseRules])

  // Search phrases in Text-Hypothesis pair

	def getRules(): List[String] = 
	{
		val returnedRules = if (Sts.opts.rulesMatchLemma)
			Sts.luceneParaphrases.map(_.query(Sts.text + " " + Sts.textLemma, Sts.hypothesis + " " + Sts.hypothesisLemma)).flatten
			else
				Sts.luceneParaphrases.map(_.query(Sts.text  , Sts.hypothesis )).flatten

				LOG.trace("Paraphrase rules before filtring: ");
		returnedRules.foreach(rule => LOG.trace(rule))


		val filterStart = System.nanoTime
		val paraphraseRules = returnedRules
			.flatMap { rule =>
				val Array(id, ruleLhs, ruleRhs, score) = rule.split("\t")
		
				var textSen = Sts.text;
				var hypSen = Sts.hypothesis;
				var lhs = ruleLhs;
				var rhs = ruleRhs;
				if (Sts.opts.rulesMatchLemma)//Some datasets use lemmas, other use non-lemmas, so, search in the sentence and the lemmatized sentence
				{
					textSen = Sts.textLemma
					hypSen = Sts.hypothesisLemma;
					lhs = Lemmatize.lemmatizeWords(ruleLhs);
					rhs = Lemmatize.lemmatizeWords(ruleRhs);
				}
				if(process(textSen).contains(process(Tokenize.separateTokens(lhs))) &&
					process(hypSen).contains(process(Tokenize.separateTokens(rhs)))
				)
					Some(rule + "\t" + RuleType.Implication);
				else if (process(hypSen).contains(process(Tokenize.separateTokens(lhs))) &&
					process(textSen).contains(process(Tokenize.separateTokens(rhs)))
				)
					Some(rule + "\t" + RuleType.BackwardImplication)
				else 
					None 
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
