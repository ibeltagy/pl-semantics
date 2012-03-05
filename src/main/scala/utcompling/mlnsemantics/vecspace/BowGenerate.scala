package utcompling.mlnsemantics.vecspace

import utcompling.mlnsemantics.util.ScoobiUtil._
import scala.collection.JavaConversions._
import utcompling.scalalogic.util.FileUtils._
import utcompling.scalalogic.util.CollectionUtils._
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.DiscourseInterpreter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MMap }
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.DList
import com.nicta.scoobi.DList._
import com.nicta.scoobi.io.text.TextInput._
import com.nicta.scoobi.io.text.TextOutput._
import java.io.File

/**
 *
 */
object BowGenerate {

  val MIN_COUNT = 50
  val NUM_FEATURES = 2000
  val WINDOW_SIZE = scala.Int.MaxValue
  val punctuation = Set(".", ",", "``", "''", "'", "`", "--", ":", ";", "-RRB-", "-LRB-", "?", "!", "-RCB-", "-LCB-", "...")

  def main(allArgs: Array[String]) = withHadoopArgs(allArgs) { args =>
    val List(inputFile, outputFile) = args.toList

    val allWordsSorted = getSortedCounts(inputFile)
    println("ALL WORDS:")
    allWordsSorted.foreach(println)
    val features = allWordsSorted.take(NUM_FEATURES).toSet

    val vectors = getSentenceContexts(inputFile, features, allWordsSorted.toSet)
    val vectorStrings = vectors.map { case (word, vector) => "%s\t%s".format(word, vector.map { case (feature, count) => "%s\t%d".format(feature, count) }.mkString("\t")) }
    persist(toTextFile(vectorStrings, outputFile))
  }

  def getSortedCounts(inputFile: String) = {
    // Get the count of each word in the corpus
    val counts: DList[(String, (Int, Int))] =
      fromTextFile(inputFile)
        .flatMap(_ // for each sentence
          .trim // remove trailing space
          .split(" ").toList // split into individual tokens
          .counts // map words to the number of times they appear in this sentence
          .map {
            // map word to its count in the sentence AND a count of 1 document 
            // that they word has appeared in. 
            case (word, count) => (word, (count, 1))
          })
        .groupByKey
        .combine { case ((tf1: Int, df1: Int), (tf2: Int, df2: Int)) => (tf1 + tf2, df1 + df2) }

    // Keep only the non-punctuation words occurring more than MIN_COUNT times
    val filteredCounts = counts.filter { case (w, (tf, df)) => tf >= MIN_COUNT && !punctuation(w) }

    // Compute TF-IDF value for each word (negated so that sorting works out)
    // (Note: We use 1 instead of the actual number of documents b/c all we care about is the ordering.) 
    val tfidfs = filteredCounts.map { case (word, (tf, df)) => (word, -tf * math.log(1.0 / df)) }

    // Sort by frequency
    val sortedCounts: DList[String] =
      tfidfs.map(_.swap)
        .groupByKey
        .flatMap(_._2)

    sortedCounts.toIterable
  }

  def getSentenceContexts(inputFile: String, features: Set[String], allWords: Set[String]) = {
    fromTextFile(inputFile)
      .map(_ // for each sentence
        .trim // remove trailing space
        .split(" ").toList) // split into individual tokens
      .flatMap { tokens => // take the list of tokens
        tokens.zipWithIndex.collect {
          case (token, i) if allWords(token) => // for each token that meets the cutoff
            val before = tokens.slice(i - WINDOW_SIZE, i) // get the tokens before it
            val after = tokens.slice(i + 1, i + 1 + WINDOW_SIZE) // and the tokens after it
            val featuresInWindow = (before ++ after).filter(features) // keep only the features in the window
            (token, featuresInWindow)
        }
      }
      .groupByKey.map { case (k, v) => (k, v.flatten.counts) } // map words to feature counts
  }
}
