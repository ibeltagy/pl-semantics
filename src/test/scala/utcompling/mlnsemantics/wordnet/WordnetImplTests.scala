package utcompling.mlnsemantics.wordnet

import org.junit.Test
import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._

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

  }

}
