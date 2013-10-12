package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.datagen.SimpleTokenizer
import utcompling.mlnsemantics.run.Sts
import scala.Array.canBuildFrom

class ParaphraseRules extends Rules{
  private val LOG = LogFactory.getLog(classOf[ParaphraseRules])

  // Search phrases in Text-Hypothesis pair

  def getRules(): List[String] = {

    val returnedRules = Sts.luceneParaphrases.query(Sts.hypothesis + " " + Sts.hypothesisLemma)

    val filterStart = System.nanoTime
    val paraphraseRules = returnedRules
      .filter { rule =>

        val Array(id, left, right, score) = rule.split("\t")
        val lhs = (" " + left + " ").replaceAll(" (a|an|the) ", " ")
        val rhs = (" " + right + " ").replaceAll(" (a|an|the) ", " ")

        val simpleTxt = (" " + SimpleTokenizer(Sts.text).mkString(" ") + " ").replaceAll(" (a|an|the) ", " ")
        val simpleHyp = (" " + SimpleTokenizer(Sts.hypothesis).mkString(" ") + " ").replaceAll(" (a|an|the) ", " ")

        val simpleLemTxt = (" " + SimpleTokenizer(Sts.textLemma).mkString(" ") + " ").replaceAll(" (a|an|the) ", " ")
        val simpleLemHyp = (" " + SimpleTokenizer(Sts.hypothesisLemma).mkString(" ") + " ").replaceAll(" (a|an|the) ", " ")

        (simpleTxt.contains(lhs) || simpleLemTxt.contains(lhs)) &&
          (simpleHyp.contains(rhs) || simpleLemHyp.contains(rhs))
      }.toList

    val filterEnd = System.nanoTime
    LOG.debug("Filtering time: " + (filterEnd - filterStart) / 1e9 + " s")
    LOG.trace("Paraphrase rules: ");
    paraphraseRules.foreach(rule => LOG.trace(rule))
    paraphraseRules
  }
}

object ParaphraseRules {
  
}
