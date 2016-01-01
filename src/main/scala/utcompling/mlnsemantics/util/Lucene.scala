package utcompling.mlnsemantics.util

import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search._
import org.apache.lucene.store._
import org.apache.lucene.util.Version
import scala.collection.JavaConversions._
import org.apache.lucene.analysis.core.SimpleAnalyzer
import java.io.File
import org.apache.commons.logging.LogFactory
import scala.io.Source
import math.{ceil, min}
import utcompling.mlnsemantics.datagen.Tokenize
import org.apache.lucene.queryparser.classic.QueryParserBase


/**
 * A companion object handles reading and writing to Lucene.
 * We use a specific SimpleAnalyzer.
 * The paraphrase rules are stored directly in Lucene.
 * This object is threadsave, but you might find a slightly outdated version.
 */
class Lucene(rulesFileName: String) {
  
  private val LOG = LogFactory.getLog(classOf[Lucene])

  var createIndex = false;
  var initIndex = false;
  val index = rulesFileName match {
    case "" => {
      initIndex = true; //In RAMDirectory need to be initialized even with an empty entry to be able to read
      new RAMDirectory();  //dummy index
    }
    case _ => {
      val file = new File(rulesFileName+".idx");
      file.exists() match {
        case true => LOG.info("Index already exists: "+ rulesFileName+".idx");
        case false => {
          LOG.info("Build lucene index: "+ rulesFileName+".idx");
          createIndex = true;
        }
      }
      new NIOFSDirectory(file);  //File-System index 
    }
  }
  var reader:DirectoryReader = null;  
  val fieldName = "text"
  val maxNumReturns = 1000000
  val analyzer = new SimpleAnalyzer(Version.LUCENE_41)
  val config = new IndexWriterConfig(Version.LUCENE_41, analyzer)
  var writer:IndexWriter = null; 
  val parser = new QueryParser(Version.LUCENE_41, fieldName, analyzer)
        
  if (initIndex || createIndex)
  {
      writer = new IndexWriter(index, config)	//<------------------------
      if (initIndex)
    	  this.write(Iterable[String]()); //initialize the writer.
      if (createIndex)
    	  writeRulesFile(rulesFileName);      
      writer.close;    	//<------------------------
  }
    
  /**
   * Write a rules file to Lucene database. (with progress bar)
   */
  def writeRulesFile(rulesFile: String) {
	val start = System.nanoTime
	val step = 100000
	//val numRules = Source.fromFile(rulesFile, "ISO-8859-1").getLines.size
	val numRules = Source.fromFile(rulesFile).getLines.size
	val itrCount = (ceil (numRules.toDouble / step)).intValue()
	for(i <- 0 to itrCount - 1)
	{
		LOG.info(i)
		//val resource = Source.fromFile(rulesFile, "ISO-8859-1")
		val resource = Source.fromFile(rulesFile)
		val from  = i * step
				val to = min((i + 1) * step, numRules)
		val rules = resource.getLines.slice(from, to).toIterable
		this.write(rules)
		resource.close
	}
	println("Finished indexing.")
	val end = System.nanoTime
	println("Indexing time: " + (end - start) / 1e9 + " s")
  }

  /**
   * Write a bag of paraphrase rules into Lucene database
   */
  def write(rules: Iterable[String]) {
    val documents = asJavaIterable(rules
      .map({ rule =>
        val doc = new Document()
        doc.add(new TextField(fieldName, rule.toLowerCase, Field.Store.YES))
        doc
      }))
    writer.addDocuments(documents)
    writer.commit()
  }

  /**
   * Read paraphrase rules satisfying the query from Lucene database
   * This function returns entries exactly matching the query string
   */
  def read(query: String): Seq[String] = {
    if(reader == null)
       reader = DirectoryReader.open(index);
    val searcher = new IndexSearcher(reader)
    // The maximum number of returned rules is set to maxNumReturns
    val collector = TopScoreDocCollector.create(maxNumReturns, true)
    searcher.search(parser.parse(query), collector)
    collector.topDocs().scoreDocs.toSeq.map(_.doc).map(searcher.doc(_).get(fieldName))
  }
   
  val ignoredTokens = List("a", "an", "the", "be", "is", "are", "to", "in", "on", "at", "of", "for")
  
  def luceneEscapeString (s:String): String =  
  {
    val specialChars = List("+", "-", "&&", "||", "!", "(", ")", "{", "}", "[", "]", "^",  "\"", "~", "*", "?", ":", "\\")
    var escapedS = s;
    for (specialChar <- specialChars)
    	escapedS = escapedS.replace(specialChar, "\\" + specialChar)
    escapedS
  }
  //This function is like "read(query)" but it cleanups the query string before querying Lucene  
  def query(q: String): Seq[String] = 
  {
	val start = System.nanoTime    
	val query = q.split(" ")
	.filter(token => token.length > 1 && !ignoredTokens.contains(token))
	.toSeq
	.distinct
	.mkString(" ")
	return this.queryWithTimeLog(query)
  }
  def query(sen1: String, sen2: String): Seq[String] = 
  {
	val query1 = sen1.split(" ")
	.filter(token => token.length > 1 && !ignoredTokens.contains(token))
	.toSeq
	.distinct
	.mkString(" ")
	val query2 = sen2.split(" ")
	.filter(token => token.length > 1 && !ignoredTokens.contains(token))
	.toSeq
	.distinct
	.mkString(" ")
	return this.queryWithTimeLog("(" + QueryParserBase.escape(query1) + ") AND (" + QueryParserBase.escape(query2) +")" )
  }
  
  def exactMatchingQuery(q: String): Seq[String] = 
  {
	return this.queryWithTimeLog("\" " + QueryParserBase.escape(q) + " \"");
  }
  
  def queryWithTimeLog(q: String): Seq[String] = 
  {
	val start = System.nanoTime    
	val r = this.read(q);
	val end = System.nanoTime
	LOG.debug("Searching time: " + (end - start) / 1e9 + " s")       
	LOG.debug("# returned rules: " + r.size)
	r;
  }
  


  /**
   * Read paraphrase rules satisfying the phrase query from Lucene database
   */
  def readPhrase(query: String): Seq[String] = {
    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    // The maximum number of returned rules is set to maxNumReturns
    val collector = TopScoreDocCollector.create(maxNumReturns, true)

    val phraseQuery = new PhraseQuery()
    query.split(" ").foreach { token =>
      phraseQuery.add(new Term(fieldName, token))
    }

    searcher.search(phraseQuery, collector)
    collector.topDocs().scoreDocs.toSeq.map(_.doc).map(searcher.doc(_).get(fieldName))
  }

}
object Lucene
{
	def main(args: Array[String])
	{
		val l = new Lucene ("resources/ppdb.small")
		val res = l.query("Some adults are sitting in the chairs and are watching the ocean ______parse_failed______")
		println(res)
		
	}
}
