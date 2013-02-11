package utcompling.mlnsemantics.vecspace

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.FileUtils._
import scala.collection.mutable.Buffer
import scala.annotation.tailrec

object BowVectorSpace {
  def apply(filename: String): Map[String, BowVector] = {
    apply(filename, _ => true)
  }

  def apply(filename: String, filter: String => Boolean): Map[String, BowVector] = {
    readLines(filename).map(_.split("\t")).flatMap {
      case Array(word, vector @ _*) =>
        if (filter(word)) {
          val pairs =
            vector.grouped(2)
              .map(_.toTuple2)
              .filter(_._2.nonEmpty)
              //.map { x => println(x); x }
              .mapVals(_.toDouble)
          if (pairs.nonEmpty)
            Some(word -> new BowVector(pairs.toMap))
          else
            None
        }
        else
          None
    }.toMap
  }
}

class BowVector(val counts: Map[String, Double]) {

  def +(other: BowVector) = {
    new BowVector((this zip2 other).map { case (k, (t, o)) => (k, t + o) })
  }
  
  def /(d: Double) = {
    new BowVector(counts.mapValues(_ / d))
  }

  def cosine(other: BowVector) = {
    val numer = (this zip other).sumBy { case ((_, t), (_, o)) => t * o }
    val denom1 = math.sqrt(this.counts.sumBy { case (_, c) => c * c })
    val denom2 = math.sqrt(other.counts.sumBy { case (_, c) => c * c })
    numer / (denom1 * denom2)
  }

  def zip(other: BowVector): Iterable[((String, Double), (String, Double))] = {
    val t = this.counts.toList.sorted.toList
    val o = other.counts.toList.sorted.toList

    @tailrec def doZip(a: List[(String, Double)], b: List[(String, Double)], accum: List[((String, Double), (String, Double))]): List[((String, Double), (String, Double))] = {
      (a, b) match {
        case ((aS, aD) :: aTail, (bS, bD) :: bTail) if aS == bS =>
          doZip(aTail, bTail, ((aS, aD), (bS, bD)) :: accum)
        case ((aS, aD) :: aTail, (bS, bD) :: bTail) if aS < bS =>
          doZip(aTail, b, ((aS, aD), (aS, 0.0)) :: accum)
        case ((aS, aD) :: aTail, (bS, bD) :: bTail) /* if aS > bS */ =>
          doZip(a, bTail, ((bS, 0.0), (bS, bD)) :: accum)
        case ((aS, aD) :: aTail, Nil) =>
          doZip(aTail, Nil, ((aS, aD), (aS, 0.0)) :: accum)
        case (Nil, (bS, bD) :: bTail) =>
          doZip(Nil, bTail, ((bS, 0.0), (bS, bD)) :: accum)
        case (Nil, Nil) =>
          accum
      }
    }

    doZip(t, o, List())
  }

  def zip2(other: BowVector): Map[String, (Double, Double)] = {
    (this.counts.keySet ++ other.counts.keySet).mapTo(k => (this.counts.getOrElse(k, 0.), other.counts.getOrElse(k, 0.))).toMap
  }

  override def toString = "BowVector(%s)".format(counts.map { case (k, v) => "%s -> %s".format(k, v) }.mkString(", "))
}
