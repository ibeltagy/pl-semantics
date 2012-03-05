package utcompling.mlnsemantics.vecspace

import utcompling.scalalogic.util.CollectionUtils._
import scala.collection.mutable.Buffer
import scala.annotation.tailrec

object BowVectorSpace {
  def apply(filename: String) = {
    io.Source.fromFile(filename).getLines.map(_.split("\t")).flatMap {
      case Array(word, vector @ _*) =>
        val pairs = vector.grouped(2).map { case Seq(feature, count) => (feature, count.toDouble) }
        if (pairs.nonEmpty)
          Some(word -> new BowVector(pairs.toMap))
        else {
          println("Empty vector: " + word)
          None
        }
    }.toMap
  }
}

class BowVector(val counts: Map[String, Double]) {
  def cosine(other: BowVector) = {
    val numer = (this zip other).sumMap { case ((_, t), (_, o)) => t * o }
    val denom1 = math.sqrt(this.counts.sumMap { case (_, c) => c * c })
    val denom2 = math.sqrt(other.counts.sumMap { case (_, c) => c * c })
    numer / (denom1 * denom2)
  }

  def zip(other: BowVector): List[((String, Double), (String, Double))] = {
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

  override def toString = "BowVector(%s)".format(counts.map { case (k, v) => "%s -> %s".format(k, v) }.mkString(", "))
}

object BowVector {
  def main(args: Array[String]) {
    {
      val a = new BowVector(Map("a" -> 1, "b" -> 2, "c" -> 3))
      val b = new BowVector(Map("b" -> 1, "c" -> 2, "d" -> 3))
      println(a zip b)
      println(b zip a)
    }
    {
      val a = new BowVector(Map("a" -> 1, "b" -> 2, "c" -> 3))
      val b = new BowVector(Map("c" -> 2, "d" -> 3, "e" -> 4))
      println(a zip b)
      println(b zip a)
    }
    {
      val a = new BowVector(Map("a" -> 1, "c" -> 3, "f" -> 4))
      val b = new BowVector(Map("b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4))
      println(a zip b)
      println(b zip a)
    }
    {
      val a = new BowVector(Map())
      val b = new BowVector(Map("b" -> 1, "c" -> 2, "d" -> 3))
      println(a zip b)
      println(b zip a)
    }
    {
      val a = new BowVector(Map("a" -> 1))
      val b = new BowVector(Map("b" -> 1, "c" -> 2, "d" -> 3))
      println(a zip b)
      println(b zip a)
    }

    {
      val a = new BowVector(Map("a" -> 1))
      val b = new BowVector(Map("b" -> 5))
      println(a cosine b)
      println(b cosine a)
    }
    {
      val a = new BowVector(Map("a" -> 1))
      val b = new BowVector(Map("a" -> 5))
      println(a cosine b)
      println(b cosine a)
    }
    {
      val a = new BowVector(Map("a" -> 1))
      val b = new BowVector(Map("a" -> 5, "b" -> 5))
      println(a cosine b)
      println(b cosine a)
    }

    val vs = BowVectorSpace("data/nytgiga.lem.vc.small")
    vs.foreach(println)
  }
}
