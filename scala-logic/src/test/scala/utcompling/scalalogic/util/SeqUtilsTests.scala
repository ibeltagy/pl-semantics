package utcompling.scalalogic.util

import org.junit.Test

class SeqUtilsTests {
  def test() {

    println(SeqUtils.partitionN((0 to 10), (i: Int) => (i % 3)))
    println(SeqUtils.partitionN(Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), (i: Int) => (i % 3)))
    println(SeqUtils.partitionN(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), (i: Int) => (i % 3)))
    println(SeqUtils.partitionN(Map(0 -> 0, 1 -> 2, 2 -> 6, 3 -> 6, 4 -> 8, 5 -> 4), (x: (Int, Int)) => (if (x._1 * 2 == x._2) 0 else 1)))

  }
}
