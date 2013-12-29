package utcompling.mlnsemantics.vecspace

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.FileUtils._
import scala.collection.mutable.Buffer
//import scala.annotation.tailrec
//import scala.{specialized => spec}
import breeze.linalg.{DenseVector, SparseVector, Vector => BrVector}
import collection.mutable.{MutableList => MList, HashMap => MHashMap}

object BowVectorSpace {
  def apply(filename: String): BowVectorSpace = {
    readSpace(filename)
  }

  def apply(filename: String, fltr: String => Boolean): BowVectorSpace = {
    val res = readSpace(filename)
      .filter(sv => fltr(sv._1))
    res
  }

  private def isDouble(s: String): Boolean = {
    try {
      s.toDouble
      true
    } catch {
      case nfe: java.lang.NumberFormatException => false
    }
  }

  def readSpace(filename: String): BowVectorSpace = {
    val reader = detectFormat(filename)
    val res = reader(filename)
    res
  }

  // auto-detects between the dhg format, a dense space and a sparse space
  // using the first line
  def detectFormat(filename: String): (String => BowVectorSpace) = {
    val firstLine = readLines(filename).next
    val Array(word, fields @ _*) = firstLine.split("\t", -1)
    if (isDouble(fields(0))) {
      // first field is a vector value. Definitely a dense space
      readDenseSpace _
    } else {
      // not a dense space. either a dhg space or a sparse space
      if (fields.length == 2) {
        // only context and weight, definitely a sparse space
        readSparseSpace _
      } else {
        readDhgSpace _
      }
    }
  }

  def readDhgSpace(filename: String): BowVectorSpace = {
    readLines(filename)
      .map(_.split("\t", -1))
      .collect {
        case Array(word, vector @ _*) => {
          val (dims, vals_s) = vector.grouped(2).map(_.toTuple2).unzip
          val (indx, vals) =
            vals_s.zipWithIndex.flatMap {
              case ("", i) => None
              case (v, i)  => Some((i, v.toDouble))
            }.sortBy(_._1).unzip
          (word, new SparseBowVector(vals, indx, dims.size))
        }
      }
      .toMap
  }

  private def changeExtension(filename: String, newExtension: String): String = {
    val position = filename.lastIndexOf('.')
    if (position < 0)
      filename + "." + newExtension
    else
      filename.substring(0, position) + newExtension
  }


  def readSparseColumns(filename: String): Map[String, Int] = {
      try {
        val colFilename = changeExtension(filename, ".cols")
        if (colFilename == filename)
          throw new java.io.IOException("Can't find cols file.")
        readLines(colFilename).zipWithIndex.toMap
      } catch {
        case e: java.io.IOException => {
          readLines(filename).map(_.split("\t")).map(_(1)).toSet.zipWithIndex.toMap
        }
      }
  }

  def readSparseSpace(filename: String): BowVectorSpace = {
    val dim2index: Map[String,Int] = readSparseColumns(filename)

    val vectors = MHashMap[String, MList[(Int, Double)]]()

    readLines(filename)
      .map(_.split("\t"))
      .foreach{ case Array(word, dim, value_s) => {
        val value = value_s.toDouble
        if (!vectors.contains(word)) {
          vectors(word) = new MList
        }
        vectors(word) += ((dim2index(dim), value_s.toDouble))
      }}

    vectors
      .mapValues { dim_val_pairs => {
        val (indx, vals) = dim_val_pairs.sortBy(_._1).unzip
        new SparseBowVector(vals, indx, dim2index.size)
      }}
      .toMap
  }

  def readDenseSpace(filename: String): BowVectorSpace = {
    readLines(filename)
      .map(_.split("\t"))
      .collect { case Array(word, vector @ _*) => (word, new DenseBowVector(vector.map(_.toDouble))) }
      .toMap
  }
}

class BowVectorWithDistances(val self: BowVector) {
  def norm2: Double = {
    self.norm(2)
  }

  def cosine(other: BowVector): Double = {
    (self dot other) / (self.norm2 * other.norm(2))
  }

  def euclid(other: BowVector): Double = {
    (self - other).norm(2)
  }
}

class SparseBowVector(vals: TraversableOnce[Double], indx: TraversableOnce[Int], numDims: Int) extends SparseVector[Double](indx.toArray, vals.toArray, numDims) {
  def this(numDims: Int) = this(Array[Double](), Array[Int](), numDims)
}



class DenseBowVector(vals: TraversableOnce[Double]) extends DenseVector[Double](vals.toArray, 0)

