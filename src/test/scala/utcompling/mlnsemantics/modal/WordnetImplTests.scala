package utcompling.mlnsemantics.modal

import java.io.File
import java.net.URL
import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._

object WordnetImplTests {
    def main(args: Array[String]): Unit = {

        val wn = new WordnetImpl()

        wn.synsets("big", POS.ADJECTIVE).map(println)
        println
        wn.synsets("sweep", POS.VERB).map(println)
        println
        wn.synsets("convertible", POS.NOUN).map(w => w.getWords.map(_.getLemma).mkString("/") + ": " +
            (wn.hypernyms(w).map(_.getWords.map(_.getLemma).mkString("/")).mkString(", "))).map(println)
        println

    }

}
