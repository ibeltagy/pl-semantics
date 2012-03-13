package utcompling.mlnsemantics.vecspace

import scala.collection.JavaConversions._
import utcompling.scalalogic.util.FileUtils._
import utcompling.scalalogic.util.CollectionUtils._
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.DiscourseInterpreter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MMap }
import java.io.File
import org.apache.log4j.Logger
import org.apache.log4j.Level
import com.cloudera.crunch.io.{ From => from }
import com.cloudera.scrunch._
import com.cloudera.scrunch.Conversions._

/**
 *
 *
 * HOW TO RUN:
 *
 * cd ~
 * vi ~/.hadoop2/conf/hadoop-env.sh: export HADOOP_HEAPSIZE=2000
 * ./fix_HDFS.sh
 * cd mln-semantics
 * sbt package-hadoop
 * hadoop fs -put /scratch/01899/dhg1/nytgiga.lem nytgiga.lem
 * hadoop jar target/mln-semantics-assembly-0.0.1.jar utcompling.mlnsemantics.vecspace.BowGenerate nytgiga.lem nytgiga.lem.vc.out
 * hadoop fs -getmerge nytgiga.lem.vc.out /scratch/01899/dhg1/nytgiga.lem.vc
 */
object BowGenerate {

  val MIN_COUNT = 50
  val NUM_FEATURES = 2000
  val WINDOW_SIZE = scala.Int.MaxValue
  val punctuation = Set(".", ",", "``", "''", "'", "`", "--", ":", ";", "-RRB-", "-LRB-", "?", "!", "-RCB-", "-LCB-", "...")

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)

    val List(inputFile, outputFile) = args.toList

    val allWordsSorted = getSortedCounts(inputFile)
    println("ALL WORDS:")
    val features = allWordsSorted.take(NUM_FEATURES).map(_._1).toSet
    allWordsSorted.grouped((allWordsSorted.size / 50) + 1).take(20).map(_.head).foreach(println)

    val vectors = getBowVectors(inputFile, features, allWordsSorted.toMap)
    val vectorStrings = vectors.map {
      case (word, vector) => "%s\t%s".format(word, vector.map {
        case (feature, count) => "%s\t%s".format(feature, count)
      }.mkString("\t"))
    }

    writeUsing(outputFile) { f =>
      for (line <- vectorStrings.materialize)
        f.write(line + "\n")
    }
  }

  def getSortedCounts(inputFile: String): Iterable[(String, Double)] = {
    val DUMMY = ""

    // Get scalar number of sentences
    val List((1, numSentences)) = new Pipeline[CountSentences].read(from.textFile(inputFile)).map(_ => 1).count.materialize.toList
    println("numSentences = " + numSentences)

    // Get the count of each word in the corpus
    val counts: PTable[String, (Int, Int)] =
      new Pipeline[GetSortedCounts]
        .read(from.textFile(inputFile))
        .map(_ // for each sentence
          .trim) // remove trailing space
        .filter(!badLine(_)) // remove weird lines
        .flatMap(_
          .split(" ").toList // split into individual tokens
          .counts // map words to the number of times they appear in this sentence
          .map {
            // map word to its count in the sentence AND a count of 1 document 
            // that they word has appeared in. 
            case (word, count) => (word, (count, 1))
          }) // add a dummy word to count the total number of sentences
        .groupByKey
        .combine { tfdfCounts =>
          val (tfCounts, dfCounts) = tfdfCounts.unzip
          (tfCounts.sum, dfCounts.sum)
        }

    // Keep only the non-punctuation words occurring more than MIN_COUNT times
    val filteredCounts = counts.filter { case (w, (tf, df)) => tf >= MIN_COUNT && !punctuation(w) }

    // Compute TF-IDF value for each word (negated so that sorting works out)
    val tfidfs = filteredCounts.map { case (word, (tf, df)) => (-tf * math.log(numSentences.toDouble / df), word) }

    // Sort by frequency
    val sortedTfidfs: PTable[String, Double] = tfidfs.groupByKey.flatMap { case (tfidf, words) => words.map(_ -> -tfidf) }

    sortedTfidfs.materialize
  }

  def badLine(s: String) = {
    Set(
      "-LRB- STORY CAN END HERE .",
      "OPTIONAL 2ND TAKE FOLLOWS . -RRB-")(s)
  }

  def getBowVectors(inputFile: String, features: Set[String], tfidfs: Map[String, Double]) = {
    new Pipeline[GetBowVectors]
      .read(from.textFile(inputFile))
      .map(_ // for each sentence
        .trim // remove trailing space
        .split(" ").toList) // split into individual tokens
      .flatMap { tokens => // take the list of tokens
        tokens.zipWithIndex.collect {
          case (token, i) if tfidfs.contains(token) => // for each token that meets the cutoff
            val before = tokens.slice(i - WINDOW_SIZE, i) // get the tokens before it
            val after = tokens.slice(i + 1, i + 1 + WINDOW_SIZE) // and the tokens after it
            val featuresInWindow = (before ++ after).filter(features) // keep only the features in the window
            (token, featuresInWindow)
        }
      }
      .groupByKey.map {
        case (word, contexts) =>
          (word, contexts.flatten.counts.map { // convert contexts to feature counts
            case (feature, count) =>
              (feature, count * tfidfs(feature)) // scale feature counts by the TF-IDF of the feature
          })
      }
  }

  class CountSentences
  class GetSortedCounts
  class GetBowVectors
}
