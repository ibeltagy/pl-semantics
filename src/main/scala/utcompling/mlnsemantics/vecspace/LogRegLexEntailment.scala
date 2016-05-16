package utcompling.mlnsemantics.vecspace

import breeze.linalg.{DenseVector, SparseVector, Vector => BrVector}
import collection.mutable.{MutableList => MList, HashMap => MHashMap}
import opennlp.scalabha.util.FileUtils._

import java.io.File

trait LexEntailmentModel {
  def entails(word1: BowVector, word2: BowVector): Double
}

class CosineLexEntailmentModel extends LexEntailmentModel {
  def entails(word1: BowVector, word2: BowVector): Double = {
    word1 cosine word2
  }
}

/*
class LogRegSupersenseLexEntailment(model_folder: String, resolveMethod: String) extends LexEntailmentModel {
  val model_folder_f = new File(model_folder)
  val model_files = model_folder_f.listFiles.filter(_.isFile)
  val models = model_files.map(x => (new LogRegLexEntailmentModel(x.toString))).toArray

  def entails(word1: BowVector, word2: BowVector): Double = {
    val probs: Array[Double] = models.map(_.entails(word1, word2))
    val est = resolveMethod match {
      case "max" => probs.max
      case "min" => probs.min
      case "avg" => probs.sum / probs.length
    }
    est
  }
}
*/

class LogRegLexEntailmentModel(model_file: String) extends LexEntailmentModel {
  val entailing_relations = List("syn", "hyper", "mero")

  val lines = readLines(model_file).map(_.split("\t"))
  val klasses = lines.collect{
    case Array(name, weights_s) => {
      val weights =weights_s.split(" ").map(_.toDouble)
      (name -> DenseVector(weights))
    }}.toMap

  val one = DenseVector.ones[Double](1)
  def class_predictions(word1: BowVector, word2: BowVector): Map[String, Double] = {
    val word1n = (word1 / word1.norm2).toDenseVector
    val word2n = (word2 / word2.norm2).toDenseVector
    val diff = (word1n - word2n).toDenseVector
    val difft = DenseVector.vertcat(diff, (diff :* diff).toDenseVector)
    /*
    val v1t = DenseVector.vertcat(word1n, (word1n :* word1n).toDenseVector)
    val v2t = DenseVector.vertcat(word2n, (word2n :* word2n).toDenseVector)
    val feature_vector = DenseVector.vertcat(one, DenseVector.vertcat(v1t, DenseVector.vertcat(v2t, difft)))
    */
    val feature_vector = DenseVector.vertcat(difft, one)
    val raw = klasses.map(x => (x._1 -> logistic(feature_vector.dot(x._2))))
    val summed = raw.values.sum
    val probs = raw.map(x => (x._1 -> x._2 / summed))
    probs
  }

  def entails(word1: BowVector, word2: BowVector): Double = {
    val preds = class_predictions(word1, word2)
    val sumpreds = entailing_relations.map(preds.get(_)).collect{case Some(x) => x}.sum

    sumpreds
  }

  private def logistic(t: Double): Double = {
    1.0 / (1.0 + math.exp(-t))
  }

}


