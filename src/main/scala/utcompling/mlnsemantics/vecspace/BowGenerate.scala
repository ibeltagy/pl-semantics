package utcompling.mlnsemantics.vecspace

import scala.collection.JavaConversions._
import utcompling.scalalogic.util.FileUtils._
import utcompling.scalalogic.util.CollectionUtils._
import utcompling.scalalogic.util.Pattern
import utcompling.scalalogic.util.Pattern.{ -> }
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.DiscourseInterpreter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MMap }
import java.io.File
import org.apache.log4j.Logger
import org.apache.log4j.Level
import scala.io.Source
import org.apache.commons.logging.LogFactory
import scala.collection.GenMap
import com.cloudera.scrunch.Conversions._
import com.cloudera.scrunch._
import Avros._

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
 * hadoop fs -put /scratch/01899/dhg1/nytgiga.lem nytgiga.lem
 * hadoop jar target/mln-semantics-assembly-0.0.1.jar utcompling.mlnsemantics.vecspace.BowGenerate nytgiga.lem nytgiga.lem.vc.out
 * hadoop fs -getmerge nytgiga.lem.vc.out /scratch/01899/dhg1/nytgiga.lem.vc
 */
object BowGenerate {
  val LOG = LogFactory.getLog(BowGenerate.getClass)

  val WINDOW_SIZE = scala.Int.MaxValue
  val punctuation = Set(".", ",", "``", "''", "'", "`", "--", ":", ";", "-RRB-", "-LRB-", "?", "!", "-RCB-", "-LCB-", "...", "-", "_")

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)
    Logger.getLogger("utcompling").setLevel(Level.DEBUG)

    val DEFAULT_NUM_FEATURES = "2000"
    val DEFAULT_MIN_WORD_COUNT = "50"
    val DEFAULT_WINDOW_SIZE = "Inf"

    var additionalArgs: List[String] = Nil
    if (args.size + additionalArgs.size < 4)
      additionalArgs ::= DEFAULT_WINDOW_SIZE
    if (args.size + additionalArgs.size < 4)
      additionalArgs ::= DEFAULT_MIN_WORD_COUNT
    if (args.size + additionalArgs.size < 4)
      additionalArgs ::= DEFAULT_NUM_FEATURES
    if (args.size + additionalArgs.size < 4)
      throw new RuntimeException("Expected arguments: inputFile, numFeatures, minWordCount, windowSize")

    val List(inputFile, numFeaturesString, minWordCountString, windowSizeString) = args.toList ++ additionalArgs.reverse
    val outputFile = "%s.vc.f%s.m%s.w%s".format(inputFile, numFeaturesString, minWordCountString, windowSizeString)
    val numFeatures = numFeaturesString.toInt
    val minWordCount = minWordCountString.toInt
    val windowSize = windowSizeString.toLowerCase match { case "inf" => Int.MaxValue; case s => s.toInt }

    LOG.info("outputFile = " + outputFile)

    val pipeline = new Pipeline[BowGenerate]
    val inputLines =
      pipeline
        .read(From.textFile(inputFile))
        .map(_.trim) // remove trailing space
        .filter(!badLine(_)) // remove weird lines

    val tfidfs = getTfidfs(inputLines, minWordCount)
    LOG.info("computed all tf-idfs")

    val topTfidfsP = tfidfs.top(numFeatures, maximize = true)
    val topTfidfs = topTfidfsP.materialize.toMap

    if (LOG.isDebugEnabled) {
      LOG.info("ALL FEATURES")
      for ((word, (tfidf /*, (tf, df)*/)) <- topTfidfs.toList.sorted.take(numFeatures))
        //LOG.info("    %s\t%f.3\t%d\t%d".format(word, tfidf, tf, df))
        LOG.info("    %s\t%f.3".format(word, tfidf))
    }

    val vectors = getBowVectors(inputLines, topTfidfs/*.mapValuesStrict(_._1)*/, windowSize)
    LOG.info("calculated all vectors")

    val featureList = topTfidfs.keys.toList.sorted
    val vectorStrings =
      vectors.map {
        case (word, vector) =>
          val vecString =
            featureList.map { f =>
              vector.get(f) match {
                case Some(v) => "%s\t%s".format(f, v)
                case None => "\t"
              }
            }.mkString("\t")
          "%s\t%s".format(word, vecString)
      }

    pipeline.writeTextFile(vectorStrings, outputFile)

    pipeline.done
  }

  def getTfidfs(inputLines: PCollection[String], minWordCount: Int) = {
    val DUMMY = ""

    // Get the count of each word in the corpus
    val countsWithDummy =
      inputLines
        .flatMap(_
          .split("\\s+").toList // split into individual tokens
          .counts // map words to the number of times they appear in this sentence
          .map {
            // map word to its count in the sentence AND a count of 1 document 
            // that they word has appeared in. 
            case (word, count) => (word, (count, 1))
          } + (DUMMY -> (1, 1))) // add a dummy word to count the total number of sentences
        .groupByKey
        .combine {
          tfdfCounts =>
            val (tfCounts, dfCounts) = tfdfCounts.unzip
            (tfCounts.sum, dfCounts.sum)
        }

    // Get scalar number of sentences
    val List(DUMMY -> (_ -> numSentences)) = countsWithDummy.filter((w, c) => w == DUMMY).materialize.toList
    println("numSentences = " + numSentences)

    // Get the real word counts (non-dummy)
    val counts = countsWithDummy.filter((w, c) => w != DUMMY)

    // Keep only the non-punctuation words occurring more than MIN_COUNT times
    val filteredCounts = counts.filter { case (w, (tf, df)) => tf >= minWordCount && !punctuation(w.toLowerCase) }

    // Compute TF-IDF value for each word
    val tfidfs = filteredCounts.mapValues { case (tf, df) => (tf * math.log(numSentences.toDouble / df) /*, (tf, df)*/ ) }

    tfidfs
  }

  def badLine(s: String) = {
    Set(
      "-LRB- STORY CAN END HERE .",
      "OPTIONAL 2ND TAKE FOLLOWS . -RRB-")(s)
  }

  def getBowVectors(inputLines: PCollection[String], tfidfs: Map[String, Double], windowSize: Int) = {
    val features = Set() ++ tfidfs.keySet //make a serializable feature set
    inputLines
      .flatMap { line => // take the line
        val tokens = line.split(" ").toList // split into individual tokens
        tokens.zipWithIndex.collect {
          case (token, i) if tfidfs.contains(token) => // for each token that meets the cutoff
            val before = tokens.slice(i - windowSize, i) // get the tokens before it
            val after = tokens.slice(i + 1, i + 1 + windowSize) // and the tokens after it
            val featuresInWindow = (before ++ after).filter(features) // keep only the features in the window
            (token, featuresInWindow)
        }
      }
      .groupByKey
      .map {
        case (word, contexts) =>
          (word, contexts.flatten.counts.map { // convert contexts to feature counts
            case (feature, count) =>
              (feature, count * tfidfs(feature)) // scale feature counts by the TF-IDF of the feature
          })
      }
  }

  //  case class TfidfTriple(var _1: Double, var _2: Int, var _3: Int) extends java.lang.Comparable[TfidfTriple] {
  //    def this() = this(0., 0, 0)
  //    def compareTo(that: TfidfTriple): Int = this._1.compareTo(that._1)
  //  }

}

class BowGenerate
