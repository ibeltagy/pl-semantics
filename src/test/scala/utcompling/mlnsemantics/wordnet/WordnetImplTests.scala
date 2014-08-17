package utcompling.mlnsemantics.wordnet

import org.junit.Test
import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import edu.mit.jwi.item.Pointer

class WordnetImplTests {

  @Test
  def test() {
    val wn = new WordnetImpl()

    println(wn.synsets("sweep", POS.VERB).flatMap(_.getWords.map(_.getLemma)).toSet)

    println(wn.synsets("car", POS.NOUN).head.getWords.head.getLemma)

    println(wn.allHypernyms(wn.synsets("car", POS.NOUN).head, 0).map(_.getWords.head.getLemma))
    println(wn.allHypernyms(wn.synsets("car", POS.NOUN).head, 1).map(_.getWords.head.getLemma))
    println(wn.allHypernyms(wn.synsets("car", POS.NOUN).head, 2).map(_.getWords.head.getLemma))
    println(wn.allHypernyms(wn.synsets("car", POS.NOUN).head, 3).map(_.getWords.head.getLemma))
    println(wn.allHypernyms(wn.synsets("car", POS.NOUN).head).map(_.getWords.head.getLemma))

    println(wn.allHyponyms(wn.synsets("car", POS.NOUN).head, 0).map(_.getWords.head.getLemma))
    println(wn.allHyponyms(wn.synsets("car", POS.NOUN).head, 1).map(_.getWords.head.getLemma))
    println(wn.allHyponyms(wn.synsets("car", POS.NOUN).head, 2).map(_.getWords.head.getLemma))
    println(wn.allHyponyms(wn.synsets("car", POS.NOUN).head).map(_.getWords.head.getLemma))
    
    println(wn.synsets("light", wn.getPos("a").head))
    println(wn.synsets("light", wn.getPos("a").head).head.getLexicalFile())
    println(wn.synsets("light", wn.getPos("a").head).head.getRelatedMap())
    println(wn.synsets("light", wn.getPos("a").head).head.getRelatedSynsets())
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.ALSO_SEE))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.ANTONYM))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.ATTRIBUTE))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.CAUSE))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.DERIVATIONALLY_RELATED))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.DERIVED_FROM_ADJ))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.ENTAILMENT))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.HOLONYM_MEMBER))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.HOLONYM_PART))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.HOLONYM_SUBSTANCE))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.HYPERNYM))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.HYPERNYM_INSTANCE))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.HYPONYM))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.HYPONYM_INSTANCE))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.MERONYM_MEMBER))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.MERONYM_PART))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.MERONYM_SUBSTANCE))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.PARTICIPLE))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.PERTAINYM))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.REGION))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.REGION_MEMBER))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.SIMILAR_TO))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.TOPIC))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.TOPIC_MEMBER))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.USAGE))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.USAGE_MEMBER))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getRelatedSynsets(Pointer.VERB_GROUP))
    println("tall " + wn.synsets("tall", POS.ADJECTIVE).head.getWords().head.getRelatedWords(Pointer.DERIVATIONALLY_RELATED))

    /*println(wn.synsets("light", wn.getPos("a").head).head.getRelatedSynsets(Pointer.ANTONYM))
    println(wn.synsets("light", wn.getPos("a").head).head.getRelatedSynsets(Pointer.ALSO_SEE))
    println(wn.synsets("light", wn.getPos("a").head).head.getRelatedSynsets(Pointer.HYPONYM))
    println(wn.synsets("car", wn.getPos("n").head).head.getRelatedSynsets(Pointer.HYPERNYM))
    println(wn.synsets("heavy", wn.getPos("a").head))
    * *
    */

  }

}
