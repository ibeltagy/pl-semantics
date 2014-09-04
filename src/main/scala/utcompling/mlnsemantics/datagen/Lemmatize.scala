package utcompling.mlnsemantics.datagen

import edu.stanford.nlp.process.Morphology

/**
 * Makes use of:
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/process/PTBTokenizer.html
 *
 * Other possible tokenizers:
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/process/WordSegmentingTokenizer.html (for chinese)
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/ie/machinereading/domains/ace/reader/RobustTokenizer.html (for robustness)
 */
class Lemmatize {
	private val morphology = new Morphology()
}

//Mainly used when parsing 
object Lemmatize {
  private val lemmatizer = new Lemmatize()
  def lemmatizeWord(s: String) = lemmatizer.morphology.stem(s);
  def lemmatizeWords(s: String) = s.split(" ").map(lemmatizeWord).mkString(" ");
}
