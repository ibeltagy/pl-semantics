package utcompling.mlnsemantics.modal

import edu.mit.jwi.item.ISynset
import edu.mit.jwi.item.POS

trait Wordnet {

  def synsets(word: String, pos: POS): List[ISynset]

  def hypernyms(synset: ISynset): List[ISynset]

  def hyponyms(synset: ISynset): List[ISynset]

}
