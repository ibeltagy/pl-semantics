package utcompling.mlnsemantics.datagen

import java.io.BufferedReader
import java.io.Reader
import java.io.StringReader

import scala.collection.JavaConversions.asScalaIterator

import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory

/**
 * Makes use of:
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/process/PTBTokenizer.html
 *
 * Other possible tokenizers:
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/process/WordSegmentingTokenizer.html (for chinese)
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/ie/machinereading/domains/ace/reader/RobustTokenizer.html (for robustness)
 */
class Tokenizer {
  def tokenize(s: String): Iterator[String] = tokenize(new StringReader(s))
  def tokenize(reader: Reader): Iterator[String] = new PTBTokenizer(new BufferedReader(reader), new WordTokenFactory, "").map(_.word)
}

object Tokenize {
  private val tokenizer = new Tokenizer()
  def apply(s: String) = tokenizer.tokenize(s)
}
