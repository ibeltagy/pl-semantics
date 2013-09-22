package utcompling.mlnsemantics.vecspace

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory


class DistRules
{
}

object DistRules
{
	private val LOG = LogFactory.getLog(classOf[DistRules])

	def apply(filename: String, txtPhrases: List[String], hypPhrases: List[String]): List[String] =
	{
		val phraseVecs = readLines(filename, "ISO-8859-1").toList
		var distRules = List[String]()

		val txtPhraseVecs = txtPhrases.map { txtPhrase =>
			val Array(id, content) = txtPhrase.split("\t")
			phraseVecs(id.toInt - 1).split("\t")
				.drop(1)
				.map(_.toDouble)
				.toList
		}
	   LOG.trace ("Found Text Phrases: ");
      txtPhrases.foreach(vec => LOG.trace(vec))

		val hypPhraseVecs = hypPhrases.map { hypPhrase =>
			val Array(id, content) = hypPhrase.split("\t")
			phraseVecs(id.toInt - 1).split("\t")
				.drop(1)
				.map(_.toDouble)
				.toList
		}

      LOG.trace ("Found Hypothesis Phrases: ");
      hypPhrases.foreach(vec => LOG.trace(vec))


		txtPhrases.zip(txtPhraseVecs).foreach { case (txtPhrase, txtPhraseVec) =>
			val Array(txtId, txtContent) = txtPhrase.split("\t")
			
			hypPhrases.zip(hypPhraseVecs).foreach { case (hypPhrase, hypPhraseVec) =>
				val Array(hypId, hypContent) = hypPhrase.split("\t")
				val newId = txtId + "_" + hypId 
				val simScore = cosine(txtPhraseVec, hypPhraseVec)
				val rule = newId + "\t" + txtContent + "\t" + hypContent + "\t" + simScore
				distRules :+= rule
			}
		}

		distRules
	}

	def cosine(vec1: List[Double], vec2: List[Double]): Double =
	{
		val numer = vec1.zip(vec2).sumBy { case (x, y) => 
			val v = x * y 
			if(v >= 0) v
			else 0.
		}
		val denom1 = math.sqrt(vec1.sumBy { x => x * x })
		val denom2 = math.sqrt(vec2.sumBy { y => y * y })
		val denom = denom1 * denom2

		if(denom > 0) numer / denom
		else 0
	}
}
