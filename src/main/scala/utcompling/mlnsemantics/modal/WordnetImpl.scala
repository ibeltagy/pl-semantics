package utcompling.mlnsemantics.modal

import edu.mit.jwi.IDictionary
import edu.mit.jwi.Dictionary
import java.net.URL
import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import edu.mit.jwi.item.IIndexWord
import edu.mit.jwi.item.ISynset
import edu.mit.jwi.item.Pointer

class WordnetImpl(dict: IDictionary) extends Wordnet {

    def this(path: String) =
        this({ val d = new Dictionary(new URL("file", null, path)); d.open(); d })

    def this() =
        this("resources/wordnet")

    def open(): IDictionary =
        if (dict.open())
            dict
        else
            throw new RuntimeException("Failed to open WordNet dictionary")

    def getIndexWord(word: String, pos: POS) =
        Option(dict.getIndexWord(word, pos))

    override def synsets(word: String, pos: POS): List[ISynset] =
        getIndexWord(word, pos).map(_.getWordIDs.map(dict.getWord(_).getSynset).toList).getOrElse(List())

    override def hypernyms(synset: ISynset) =
        synset.getRelatedSynsets(Pointer.HYPERNYM).map(dict.getSynset(_)).toList

    override def hyponyms(synset: ISynset) =
        synset.getRelatedSynsets(Pointer.HYPONYM).map(dict.getSynset(_)).toList
}
