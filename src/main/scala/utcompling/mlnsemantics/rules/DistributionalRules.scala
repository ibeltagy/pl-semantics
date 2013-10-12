package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.datagen.SimpleTokenizer
import utcompling.mlnsemantics.run.Sts
import scala.Array.canBuildFrom

class DistributionalRules extends Rules{
  
  private val LOG = LogFactory.getLog(classOf[DistributionalRules])

  def getRules(): List[String] = {
    // Search phrases in Text
    val txtPhrases = Sts.luceneDistPhrases.query(Sts.text + " " + Sts.textLemma)
      .filter { phrase =>

        val Array(id, content) = phrase.split("\t")
        val str = " " + content + " "

        val simpleTxt = (" " + SimpleTokenizer(Sts.text).mkString(" ") + " ")
        val simpleLemTxt = (" " + SimpleTokenizer(Sts.textLemma).mkString(" ") + " ")

        val cond = simpleTxt.contains(str) || simpleLemTxt.contains(str)

        val extraStr = " some" + str
        val extraCond = !simpleTxt.contains(extraStr) && !simpleLemTxt.contains(extraStr)

        cond && extraCond
      }.toList

    // Search phrases in Hypothesis
    val hypPhrases = Sts.luceneDistPhrases.query(Sts.hypothesis + " " + Sts.hypothesisLemma)
      .filter { phrase =>

        val Array(id, content) = phrase.split("\t")
        val str = " " + content + " "

        val simpleHyp = (" " + SimpleTokenizer(Sts.hypothesis).mkString(" ") + " ")
        val simpleLemHyp = (" " + SimpleTokenizer(Sts.hypothesisLemma).mkString(" ") + " ")

        val cond = simpleHyp.contains(str) || simpleLemHyp.contains(str)

        val extraStr = " some" + str
        val extraCond = !simpleHyp.contains(extraStr) && !simpleLemHyp.contains(extraStr)

        cond && extraCond
      }.toList

    // Compute similaritis between phrases and generate corresponding rules in the following format: 
    // <id> TAB <text_phrase> TAB <hypo_phrase> TAB <sim_score>
    val distRules = generatePairs(txtPhrases, hypPhrases);
    LOG.trace("Distributional rules: ");
    distRules.foreach(rule => LOG.trace(rule))
    return distRules;
  }

  private def generatePairs(txtPhrases: List[String], hypPhrases: List[String]): List[String] =
  {
      //phrase vectors
	  if (Sts.opts.phraseVecsFile == "")
        return List[String]();

      //val phraseVecs = readLines(filename, "ISO-8859-1").toList
      val phraseVecs = readLines(Sts.opts.phraseVecsFile).toList
      var distRules = List[String]()

      val txtPhraseVecs = txtPhrases.map { txtPhrase =>
        val Array(id, content) = txtPhrase.split("\t")
        phraseVecs(id.toInt - 1).split("\t")
          .drop(1)
          .map(_.toDouble)
          .toList
      }
      LOG.trace("Found Text Phrases: ");
      txtPhrases.foreach(vec => LOG.trace(vec))

      val hypPhraseVecs = hypPhrases.map { hypPhrase =>
        val Array(id, content) = hypPhrase.split("\t")
        phraseVecs(id.toInt - 1).split("\t")
          .drop(1)
          .map(_.toDouble)
          .toList
      }

      LOG.trace("Found Hypothesis Phrases: ");
      hypPhrases.foreach(vec => LOG.trace(vec))

      txtPhrases.zip(txtPhraseVecs).foreach {
        case (txtPhrase, txtPhraseVec) =>
          val Array(txtId, txtContent) = txtPhrase.split("\t")

          hypPhrases.zip(hypPhraseVecs).foreach {
            case (hypPhrase, hypPhraseVec) =>
              val Array(hypId, hypContent) = hypPhrase.split("\t")
              val newId = txtId + "_" + hypId
              val simScore = cosine(txtPhraseVec, hypPhraseVec)
              val rule = newId + "\t" + txtContent + "\t" + hypContent + "\t" + simScore
              distRules :+= rule
          }
      }

      distRules
    }

  private def cosine(vec1: List[Double], vec2: List[Double]): Double =
  {
      val numer = vec1.zip(vec2).sumBy {
        case (x, y) =>
          val v = x * y
          if (v >= 0) v
          else 0.
      }
      val denom1 = math.sqrt(vec1.sumBy { x => x * x })
      val denom2 = math.sqrt(vec2.sumBy { y => y * y })
      val denom = denom1 * denom2

      if (denom > 0) numer / denom
      else 0
  }
}

object DistributionalRules{
  
}