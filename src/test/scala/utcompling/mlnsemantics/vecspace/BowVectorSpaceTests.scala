package utcompling.mlnsemantics.vecspace

import org.junit.Test
import org.junit.Assert._

class BowVectorSpaceTests {

  @Test
  def test_baseballHockey() = {

    val words = Set("baseball", "hockey", "outfield", "puck")

    val vs = BowVectorSpace("resources/nytgiga.lem.1m.vc.f2000.m50.wInf", words)

    val baseball = vs.get("baseball")
    val hockey = vs.get("hockey")
    val outfield = vs.get("outfield")
    val puck = vs.get("puck")

    assertOptionDoubleEquals(Some(0.2272), for (a <- baseball; b <- outfield) yield (a cosine b))
    assertOptionDoubleEquals(Some(0.1289), for (a <- baseball; b <- puck) yield (a cosine b))
    assertOptionDoubleEquals(Some(0.0441), for (a <- hockey; b <- outfield) yield (a cosine b))
    assertOptionDoubleEquals(Some(0.2906), for (a <- hockey; b <- puck) yield (a cosine b))

  }

  @Test
  def test_BowVector_zip = {
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
  }

  @Test
  def test_BowVector_cosine = {
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

  def assertOptionDoubleEquals(expected: Option[Double], found: Option[Double]) = {
    (expected, found) match {
      case (Some(e), Some(f)) => assertEquals(e, f, 0.0001)
      case _ => assertEquals(expected, found)
    }
  }

}
