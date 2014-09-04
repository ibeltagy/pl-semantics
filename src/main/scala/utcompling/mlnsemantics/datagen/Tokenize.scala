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
class Tokenize {
  def tokenize(s: String): Iterator[String] = tokenize(new StringReader(s))
  def tokenize(reader: Reader): Iterator[String] = new PTBTokenizer(new BufferedReader(reader), new WordTokenFactory, "").map(_.word)
}

//Mainly used when parsing 
object Tokenize {
  private val tokenizer = new Tokenize()
  def apply(s: String) = tokenizer.tokenize(s)
  def separateTokens(s: String) = tokenizer.tokenize(s).mkString(" ");
}

/*def sepTokens(a: String)= {
  //Tokenize(a.replace("-","" ).replace("\"", " ").replace("\'", " ").replace("‘", " ").replace("’", " ").replace("/", " ").replace("“", " ").replace("”", " ").replace(")", " ").replace("(", " ")).mkString(" ");
  //remove non-ascii characters
  //remove control 
  Tokenize(a).mkString(" ");
	//Tokenize("""-|'|`|‘|’|/|"|“|”|\)|\(|&|>|<|=|\$|:|\+""".r.replaceAllIn(a, " ").filterNot(  (c:Char) => ( c > 127)  )).mkString(" ");
  //Tokenize(a.replace("-","" )).mkString(" ");
} */


/**
 * A very simple tokenizer that pulls most puncuation off the characters.
 * Given a raw string, tokenize it with a simple regular expression, returning
 * an IndexedSeq[String] with one token per element.
 */
//Mainly used when indexing text in Lucene
object SimpleTokenizer {
  def apply(text: String): IndexedSeq[String] = text
    .replaceAll("""([\?!\";\|\[\].,'_<>:\+\*-/&\^%\$#@~`=\(\)\d\\])""", " ")
    .toLowerCase
    .trim
    .split("\\s+")
    .toIndexedSeq
}

