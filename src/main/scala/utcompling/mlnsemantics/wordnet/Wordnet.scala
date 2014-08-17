package utcompling.mlnsemantics.wordnet

import edu.mit.jwi.item.ISynset
import edu.mit.jwi.item.POS

trait Wordnet {

  def synsets(word: String, pos: POS): List[ISynset]

  def hypernyms(synset: ISynset): List[ISynset]
  def allHypernyms(synset: ISynset): Set[ISynset]
  def allHypernyms(synset: ISynset, searchLevels: Int): Set[ISynset]

  def hyponyms(synset: ISynset): List[ISynset]
  def allHyponyms(synset: ISynset): Set[ISynset]
  def allHyponyms(synset: ISynset, searchLevels: Int): Set[ISynset]
   
  def getSynonyms(name: String, pos: String): Set[String]
  def getHypernyms(name: String, pos: String): Set[String]
  def getHyponyms(name: String, pos: String): Set[String]
  def getPos(s: String): List[POS]
}
