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

  def assertOptionDoubleEquals(expected: Option[Double], found: Option[Double]) = {
    (expected, found) match {
      case (Some(e), Some(f)) => assertEquals(e, f, 0.0001)
      case _ => assertEquals(expected, found)
    }
  }

}
