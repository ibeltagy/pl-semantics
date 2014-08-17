package utcompling.mlnsemantics.wordnet

import edu.mit.jwi.item.ISynset
import edu.mit.jwi.item.POS
import edu.mit.jwi.item.Pointer
import edu.mit.jwi.Dictionary
import edu.mit.jwi.IDictionary
import java.net.URL
import scala.collection.JavaConversions.asScalaBuffer
import utcompling.Resources
import edu.mit.jwi.item.IWord

class WordnetImpl(dict: IDictionary) extends Wordnet {

  def this(path: String) =
    this({ val d = new Dictionary(new URL("file", null, path)); d.open(); d })

  def this() =
    //this(System.getenv("WORDNETHOME"))
    this(Resources.wordnet)

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
    synset.getRelatedSynsets(Pointer.HYPERNYM).map(dict.getSynset).toList

  override def allHypernyms(synset: ISynset): Set[ISynset] = {
    allHypernyms(synset, Int.MaxValue)
  }

  override def allHypernyms(synset: ISynset, searchLevels: Int): Set[ISynset] = {
    val thisLevel = hypernyms(synset).toSet
    thisLevel ++
      (searchLevels match {
        case 0 => Nil
        case _ => thisLevel.flatMap(allHypernyms(_, searchLevels - 1))
      })
  }

  override def hyponyms(synset: ISynset) =
    synset.getRelatedSynsets(Pointer.HYPONYM).map(dict.getSynset).toList

  override def allHyponyms(synset: ISynset): Set[ISynset] = {
    allHyponyms(synset, Int.MaxValue)
  }

  override def allHyponyms(synset: ISynset, searchLevels: Int): Set[ISynset] = {
    val thisLevel = hyponyms(synset).toSet
    thisLevel ++
      (searchLevels match {
        case 0 => Nil
        case _ => thisLevel.flatMap(allHyponyms(_, searchLevels - 1))
      })
  }
  
  
  def antonyms(synset: ISynset):Set[IWord] =
		  synset.getWords().map(w=> w.getRelatedWords(Pointer.ANTONYM)).flatten.map(wIdx=>dict.getWord(wIdx)).toSet
  
  def getSynonyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- this.synsets(name, p);
      w <- s.getWords
    ) yield w.getLemma).toSet

  def getHypernyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- this.synsets(name, p);
      h <- this.allHypernyms(s, 20);
      w <- h.getWords
    ) yield w.getLemma).toSet

  def getHyponyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- this.synsets(name, p);
      h <- this.allHyponyms(s, 20);
      w <- h.getWords
    ) yield w.getLemma).toSet
    
  def getAntonyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- this.synsets(name, p);
      w <- this.antonyms(s)
    ) yield w.getLemma()).toSet
    
  def getPos(s: String): List[POS] =
    s match {
      case "n" => List(POS.NOUN)
      case "v" => List(POS.VERB)
      case "a" => List(POS.ADJECTIVE, POS.ADVERB)
      case _ => Nil
    }

}
