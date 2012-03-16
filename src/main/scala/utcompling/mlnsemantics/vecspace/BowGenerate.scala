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
  val punctuation = Set(".", ",", "``", "''", "'", "`", "--", ":", ";", "-RRB-", "-LRB-", "?", "!", "-RCB-", "-LCB-", "...")

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)
    Logger.getLogger("utcompling").setLevel(Level.DEBUG)

    var additionalArgs: List[String] = Nil
    if (args.size == 0)
      throw new RuntimeException("Expected arguments: inputFile, outputFile, numFeatures, minWordCount")
    if (args.size < 5)
      additionalArgs ::= "2000" // minWordCount
    if (args.size < 4)
      additionalArgs ::= "50" // numFeatures
    if (args.size < 3)
      additionalArgs ::= "1000000000" // windowSize
    if (args.size < 2)
      additionalArgs ::= args(0) + ".vc" // outputFile

    val List(inputFile, outputFile, windowSizeString, numFeaturesString, minWordCountString) = args.toList ++ additionalArgs
    val windowSize = windowSizeString.toInt
    val numFeatures = numFeaturesString.toInt
    val minWordCount = minWordCountString.toInt

    val pipeline = new Pipeline[BowGenerate]
    val inputLines =
      pipeline
        .read(From.textFile(inputFile))
        .map(_.trim) // remove trailing space
        .filter(!badLine(_)) // remove weird lines

    val tfidfs = getTfidfs(inputLines, minWordCount)
    LOG.info("computed all tf-idfs")

    val topTfidfs = tfidfs.top(numFeatures, maximize = true).materialize.toList

    if (LOG.isDebugEnabled) {
      LOG.debug("ALL FEATURES")
      for (x <- topTfidfs.take(numFeatures))
        LOG.debug("    " + x)
    }

    val features = topTfidfs.map(_._1).toSet
    LOG.debug("identified features")

    val vectors = getBowVectors(inputLines, features, topTfidfs.toMap, windowSize)
    LOG.debug("calculated all vectors")

    val vectorStrings =
      vectors.map {
        case (word, vector) =>
          "%s\t%s".format(word, vector.map {
            case (feature, count) => "%s\t%s".format(feature, count)
          }.mkString("\t"))
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
    val filteredCounts = counts.filter { case (w, (tf, df)) => tf >= minWordCount && !punctuation(w) }

    // Compute TF-IDF value for each word
    val tfidfs = filteredCounts.mapValues { case (tf, df) => tf * math.log(numSentences.toDouble / df) }

    tfidfs
  }

  def badLine(s: String) = {
    Set(
      "-LRB- STORY CAN END HERE .",
      "OPTIONAL 2ND TAKE FOLLOWS . -RRB-")(s)
  }

  def getBowVectors(inputLines: PCollection[String], features: Set[String], tfidfs: Map[String, Double], windowSize: Int) = {
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
      .ungroup
      .mapValues {
        _.counts.map { // convert contexts to feature counts
          case (feature, count) =>
            (feature, count * tfidfs(feature)) // scale feature counts by the TF-IDF of the feature
        }
      }
  }

}

class BowGenerate
