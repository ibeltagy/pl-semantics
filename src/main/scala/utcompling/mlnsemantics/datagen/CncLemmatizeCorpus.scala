package utcompling.mlnsemantics.datagen

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path
import org.apache.hadoop.io.{ IntWritable, Text }
import org.apache.hadoop.mapreduce.{ Job, Mapper, Reducer }
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat
import org.apache.hadoop.util.GenericOptionsParser
import scala.collection.JavaConversions._
import utcompling.scalalogic.util.FileUtils._
import utcompling.scalalogic.util.CollectionUtils._
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.DiscourseInterpreter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MMap }

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

    val tokenized = sentences.map(s => Tokenize(s).mkString(" "))
    val lemmatized = parseToLemmas(tokenized)
    val lemmasOnly = lemmatized.flatten.map(_.map(_._2).mkString(" "))

    returnKey.set(batchNum.toInt)
    returnVal.set(lemmasOnly.mkString("\t"))
    context.write(returnKey, returnVal)
  }

  private def parseToLemmas(sentences: Seq[java.lang.String]): Seq[Option[List[(String, String)]]] = {
    val candcArgs = Map[String, String](
      "--candc-printer" -> "boxer")
    val candcOut = candc.batchParse(sentences, candcArgs, None, Some("boxer"))
    val outputs = splitOutput(candcOut)
    val lemmatized = lemmatize(outputs)
    sentences.indices.map(lemmatized.get)
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
      } else if (currNum.isDefined) {
        if (line.trim.isEmpty) {
          outputs += (currNum.get -> current.mkString("\n"))
          current.clear()
          currNum = None
        } else
          current += line
      }
    }
    outputs.toMap
  }

  private def lemmatize(outputs: Map[Int, String]): Map[Int, List[(String, String)]] = {
    val TerminalRe = """.*t\(\S+, ?'(\S+)', ?'(\S+)', ?'\S+', ?'\S+', ?'\S+'\).*""".r
    outputs.mapValuesStrict(_.split("\n").collect { case TerminalRe(word, lemma) => (cleanEscaped(word), cleanEscaped(lemma)) }.toList)
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
      } else { //if(inquote)
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
