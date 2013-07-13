package utcompling.mlnsemantics.datagen

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path
import org.apache.hadoop.io.{ IntWritable, Text }
import org.apache.hadoop.mapreduce.{ Job, Mapper, Reducer }
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat
import org.apache.hadoop.util.GenericOptionsParser
import scala.collection.JavaConversions._
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.DiscourseInterpreter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MMap }

/**
 *
 *
 * HOW TO RUN:
 *
 * cd ~
 * vi ~/.hadoop2/conf/hadoop-env.sh: export HADOOP_HEAPSIZE=2000
 * ./fix_HDFS.sh
 * cd mln-semantics
 * sbt assembly
 * hadoop fs -put /scratch/01899/dhg1/nytgiga.grouped nytgiga.grouped
 * hadoop jar target/mln-semantics-assembly-0.0.1.jar utcompling.mlnsemantics.datagen.CncLemmatizeCorpus nytgiga.grouped nytgiga.lem
 * hadoop fs -getmerge nytgiga.lem.out /scratch/01899/dhg1/nytgiga.lem
 */
object CncLemmatizeCorpus {
  def main(args: Array[String]) {
    val List(inputFile, outputFile) = args.toList

    val conf = new Configuration()
    val job = new Job(conf, "CncLemmatizeCorpus")
    job.setJarByClass(classOf[CncLemmatizeCorpusMapper])
    job.setMapperClass(classOf[CncLemmatizeCorpusMapper])
    job.setReducerClass(classOf[CncLemmatizeCorpusReducer])
    job.setOutputKeyClass(classOf[IntWritable])
    job.setOutputValueClass(classOf[Text])
    job.setNumReduceTasks(1)
    FileInputFormat.addInputPath(job, new Path(inputFile))
    FileOutputFormat.setOutputPath(job, new Path(outputFile + ".out"))
    if (!job.waitForCompletion(true))
      throw new RuntimeException("Job Failed")
  }

}

class CncLemmatizeCorpusMapper extends Mapper[Object, Text, IntWritable, Text] {
  private[this] val candc = CandcImpl.findBinary(Some(pathjoin(System.getenv("HOME"), "bin/candc/bin")))

  private[this] val returnKey = new IntWritable
  private[this] var returnVal = new Text

  override def map(key: Object, value: Text, context: Mapper[Object, Text, IntWritable, Text]#Context) {
    val Array(batchNum, sentences @ _*) = value.toString.split("\t")

    val lemmatized = parseToLemmas(sentences)
    val lemmasOnly = lemmatized.flatten.map(_.map(_._2).mkString(" "))

    returnKey.set(batchNum.toInt)
    returnVal.set(lemmasOnly.mkString("\t"))
    context.write(returnKey, returnVal)
  }

  def parseToLemmas(sentences: Seq[java.lang.String]): Seq[Option[List[(String, String)]]] = {
    val tokenized = sentences.map(s => Tokenize(s).mkString(" "))
    val candcArgs = Map[String, String](
      "--candc-printer" -> "boxer",
	"--candc-int-betas" -> "0.00075 0.0003 0.0001 0.00005 0.00001")
    val candcOut = candc.batchParse(tokenized, candcArgs, None, Some("boxer"))
    val outputs = splitOutput(candcOut)
    val lemmatized = lemmatize(outputs)
    val wordsAndLemmas = tokenized.indices.map(idx => lemmatized.get(100 * idx + 99))
//	val wordsAndLemmas = lemmatized.keys.toSeq.sorted.map(lemmatized.get)
    wordsAndLemmas
  }

  private def splitOutput(candcOut: String): Map[Int, String] = {
    val outputs = MMap[Int, String]()
    val current = new ListBuffer[String]
    var currNum: Option[Int] = None
    for (line <- candcOut.split("\n")) {
      if (line.startsWith("ccg(")) {
        currNum = Some(line.drop(4).dropRight(1).toInt - 1)
        current.clear()
        current += line
      }
      else if (currNum.isDefined) {
        if (line.trim.isEmpty || line.startsWith("id(")) {
          outputs += (currNum.get -> current.mkString("\n"))
          current.clear()
          currNum = None
        }
        else
          current += line
      }
    }
    outputs.toMap
  }

  private def lemmatize(outputs: Map[Int, String]): Map[Int, List[(String, String)]] = {
    val TerminalRe = """.*t\(\S+, ?'(\S+)', ?'(\S+)', ?'\S+', ?'\S+', ?'\S+'\).*""".r
    outputs.mapVals(_.split("\n").collect { case TerminalRe(word, lemma) => (cleanEscaped(word), cleanEscaped(lemma)) }.toList)
  }

  private def cleanEscaped(s: String) = {
    val len = s.length
    val out = new ListBuffer[Char]
    var inquote = false
    for (i <- 0 until len) {
      if (!inquote) {
        if (s(i) == '\\')
          inquote = true
        else
          out += s(i)
      }
      else { //if(inquote)
        out += s(i)
        inquote = false
      }
    }
    out.mkString("")
  }
}

class CncLemmatizeCorpusReducer extends Reducer[IntWritable, Text, Text, Text] {
  val empty = new Text("")
  val result = new Text

  override def reduce(key: IntWritable, values: java.lang.Iterable[Text], context: Reducer[IntWritable, Text, Text, Text]#Context) {
    for (sentence <- values.flatMap(_.toString.split("\t"))) {
      result.set(sentence)
      context.write(result, empty)
    }
  }
}
