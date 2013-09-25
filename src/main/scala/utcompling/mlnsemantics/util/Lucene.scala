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

/**
 * A companion object handles reading and writing to Lucene.
 * We use a specific SimpleAnalyzer.
 * The paraphrase rules are stored directly in Lucene.
 * This object is threadsave, but you might find a slightly outdated version.
 */
class Lucene(rulesFileName: String) {
  
  private val LOG = LogFactory.getLog(classOf[Lucene])

  var createIndex = false;
  val index = rulesFileName match {
    case "" => new RAMDirectory();  //dummy index
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
  val fieldName = "text"
  val maxNumReturns = 1000000
  val analyzer = new SimpleAnalyzer(Version.LUCENE_41)
  val config = new IndexWriterConfig(Version.LUCENE_41, analyzer)
  val writer = new IndexWriter(index, config)
  val parser = new QueryParser(Version.LUCENE_41, fieldName, analyzer)
  
  this.write(Iterable[String]()); //initialize the writer.
  
  if (createIndex)
	  writeRulesFile(rulesFileName);
    
  /**
   * Write a rules file to Lucene database. (with progress bar)
   */
  def writeRulesFile(rulesFile: String) {
	val start = System.nanoTime
	val step = 100000
	//val numRules = Source.fromFile(rulesFile, "ISO-8859-1").getLines.size
	val numRules = Source.fromFile(rulesFile).getLines.size
	val itrCount = (Math.ceil (numRules.toDouble / step)).intValue()
	for(i <- 0 to itrCount - 1)
	{
		LOG.info(i)
		//val resource = Source.fromFile(rulesFile, "ISO-8859-1")
		val resource = Source.fromFile(rulesFile)
		val from  = i * step
	  		val to = Math.min((i + 1) * step, numRules)
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
   */
  def read(query: String): Seq[String] = {
    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    // The maximum number of returned rules is set to maxNumReturns
    val collector = TopScoreDocCollector.create(maxNumReturns, true)
    searcher.search(parser.parse(query), collector)
    collector.topDocs().scoreDocs.toSeq.map(_.doc).map(searcher.doc(_).get(fieldName))
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
