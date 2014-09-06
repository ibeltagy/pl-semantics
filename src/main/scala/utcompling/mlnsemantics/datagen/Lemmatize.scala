package utcompling.mlnsemantics.datagen

import edu.stanford.nlp.process.Morphology

class Lemmatize {
	private val morphology = new Morphology()
}
 
object Lemmatize {
  private val lemmatizer = new Lemmatize()
  def lemmatizeWord(s: String) = lemmatizer.morphology.stem(s);
  def lemmatizeWords(s: String) = s.split(" ").map(lemmatizeWord).mkString(" ");
}
