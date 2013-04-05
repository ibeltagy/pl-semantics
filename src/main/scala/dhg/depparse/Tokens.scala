package dhg.depparse

import java.io.BufferedReader
import java.io.Reader
import java.io.StringReader
import scala.collection.JavaConversions._
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory
import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.DocumentPreprocessor

/**
 * Makes use of:
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/process/PTBTokenizer.html
 *
 * Other possible tokenizers:
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/process/WordSegmentingTokenizer.html (for chinese)
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/ie/machinereading/domains/ace/reader/RobustTokenizer.html (for robustness)
 */
object Tokens {
  def apply(s: String): IndexedSeq[String] = apply(new StringReader(s))
  def apply(reader: Reader): IndexedSeq[String] = tokenizeToWords(reader).map(_.word)
  def tokenizeToWords(s: String): IndexedSeq[Word] = tokenizeToWords(new StringReader(s))
  def tokenizeToWords(reader: Reader): IndexedSeq[Word] = new PTBTokenizer(new BufferedReader(reader), new WordTokenFactory, "").toIndexedSeq
}

object Tokenized {
  def apply(s: String): Iterator[IndexedSeq[String]] = apply(new StringReader(s))
  def apply(reader: Reader): Iterator[IndexedSeq[String]] = new DocumentPreprocessor(reader).iterator.map(_.map(_.word).toIndexedSeq)
}
