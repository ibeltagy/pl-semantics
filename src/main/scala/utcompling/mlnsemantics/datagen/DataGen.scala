package utcompling.mlnsemantics.datagen

import io.Source
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal._
import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import edu.mit.jwi.item.ISynset

object DataGen {
  def findFactives() = {
    //    val signatures = List("pn", "np", "fact_n")
    //    val interestingVerbs =
    //      (for (
    //        line <- Source.fromFile("resources/polarity-lexicon/polarity_lexicon_expanded.txt").getLines if line.trim.nonEmpty && !line.startsWith("#");
    //        Array(word, subcat, sig, example) = line.split('\t') if signatures.exists(x => sig.contains(x))
    //      ) yield word).toSet

    val natlogInterpreter = new ModalDiscourseInterpreter()
    def boxerInterpreter(x: BoxerExpression) = { new Boxer2DrtExpressionInterpreter().interpret(new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(x)) }
    val wordnet = new WordnetImpl()

    val test = Iterator(
      "John did not forget to leave .",
      "John forgot that Bill left .",
      "John did not forget to leave and did not manage to care .")
    val sts_short = new StsReader("resources/semantic-textual-similarity/STS.input.MSRvid.txt").flatten
    val sts_long = new StsReader("resources/semantic-textual-similarity/STS.input.MSRpar.txt").flatten

    def getLemmas(synsets: Iterable[ISynset]) = synsets.flatMap(_.getWords.map(_.getLemma).filterNot(_.contains("_"))).toSet

    for (sentence <- test ++ sts_short ++ sts_long) {
      println(sentence)
      for (
        word <- Tokenize(sentence) if !List("not", "in", "are", "be", "is", "a").contains(word);
        pos <- List(POS.NOUN, POS.VERB, POS.ADJECTIVE, POS.ADVERB)
      ) {
        val synsets = wordnet.synsets(word, pos)
        if (synsets.nonEmpty) {
          val synonyms = getLemmas(synsets) - word

          val hypernymSets = synsets.flatMap(synset => wordnet.hypernyms(synset)).toSet
          val hypernyms = getLemmas(hypernymSets) - word
          val hyperhyperSets = hypernymSets.flatMap(synset => wordnet.hypernyms(synset)).toSet
          val hyperhypernyms = getLemmas(hyperhyperSets) - word

          val hyponymSets = synsets.flatMap(synset => wordnet.hyponyms(synset)).toSet
          val hyponyms = getLemmas(hyponymSets) - word
          val hypohypoSets = hyponymSets.flatMap(synset => wordnet.hyponyms(synset)).toSet
          val hypohyponyms = getLemmas(hypohypoSets) - word

          println("    > %s (%s): %s".format(word, pos, synonyms.toList.sorted.mkString(", ")))
          if (hypernyms.nonEmpty) println("        hyper: %s".format(hypernyms.toList.sorted.mkString(", ")))
          if (hyperhypernyms.nonEmpty) println("        hyperhyper: %s".format(hyperhypernyms.toList.sorted.mkString(", ")))
          if (hyponyms.nonEmpty) println("        hypo: %s".format(hyponyms.toList.sorted.mkString(", ")))
          if (hypohyponyms.nonEmpty) println("        hypohypo: %s".format(hypohyponyms.toList.sorted.mkString(", ")))
        }
      }
      natlogInterpreter.process(List(List(sentence))) match {
        case List(Some((boxerEx, natlogRules))) =>
          if (natlogRules.size > 1) {
            val drs = boxerInterpreter(boxerEx)
            drs.pprint()
          }
        case List(None) =>
      }
      println()
    }

  }

  def main(args: Array[String]): Unit = {
    findFactives()
  }
}
