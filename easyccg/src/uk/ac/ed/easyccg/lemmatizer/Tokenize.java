package uk.ac.ed.easyccg.lemmatizer;

import java.io.BufferedReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.List;

import edu.stanford.nlp.ling.Word;
import edu.stanford.nlp.process.PTBTokenizer;
import edu.stanford.nlp.process.WordTokenFactory;

/**
 * Makes use of:
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/process/PTBTokenizer.html
 *
 * Other possible tokenizers:
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/process/WordSegmentingTokenizer.html (for chinese)
 * http://www-nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/ie/machinereading/domains/ace/reader/RobustTokenizer.html (for robustness)
 */
public class Tokenize {
  //private static PTBTokenizer tokenizer = new PTBTokenizer(
  //def tokenize(s: String): Iterator[String] = tokenize(new StringReader(s))
  //def tokenize(reader: Reader): Iterator[String] = new PTBTokenizer(new BufferedReader(reader), new WordTokenFactory, "").map(_.word)
  
  //private val tokenizer = new Tokenize()
  //def apply(s: String) = tokenizer.tokenize(s)
  public static String separateTokens(String s)
  {
	  PTBTokenizer<Word> t = new PTBTokenizer<Word>(new BufferedReader(new StringReader(s)), new WordTokenFactory(), "");
	  StringBuffer stringBuffer = new StringBuffer();
	  for (Word w; t.hasNext(); )
	  {
		  w = t.next();
		  if (stringBuffer.length() == 0)
			  stringBuffer.append(w.word());
		  else 
			  stringBuffer.append(" " + w.word());
	  }
	  return stringBuffer.toString();
  }
  public static void main (String[] args)
  {
	  System.out.println(separateTokens("A man is driving the man's car, the dog's cat and he isn't eating."));
  }
}
