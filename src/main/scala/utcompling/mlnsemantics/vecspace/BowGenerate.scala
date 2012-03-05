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

    new File(outputFile).recursiveDelete() // TODO: REMOVE

    val sortedCounts = getSortedCounts(inputFile)
    val allWords = sortedCounts.map(_._2).toSet
    val features = sortedCounts.take(NUM_FEATURES).map(_._2).toSet
    features.foreach(println)

    val vectors = getSentenceContexts(inputFile, features, allWords)
    val vectorStrings = vectors.map { case (word, vector) => "%s\t%s".format(word, vector.map { case (feature, count) => "%s\t%d".format(feature, count) }.mkString("\t")) }
    persist(toTextFile(vectorStrings, outputFile))

    //    vectorStrings.toIterable.foreach(println)

    //val strings = counts.map { case (w, c) => "%s\t%d".format(w, c) }

  }

  def getSortedCounts(inputFile: String) = {
    // Get the count of each word in the corpus
    val counts: DList[(String, Int)] =
      fromTextFile(inputFile)
        .flatMap(_.trim.split(" "))
        .map(word => (word, -1))
        .groupByKey
        .combine(_ + _)

    // Keep only the non-punctuation words occurring more than MIN_COUNT times
    val filteredCounts = counts.filter { case (w, c) => -c >= MIN_COUNT && !punctuation(w) }

    // Sort by frequency
    val sortedCounts: DList[(Int, String)] =
      filteredCounts.map(_.swap)
        .groupByKey
        .flatMap { case (count, words) => words.map(word => (-count, word)) }

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
