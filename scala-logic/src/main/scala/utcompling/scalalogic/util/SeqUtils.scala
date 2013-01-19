package utcompling.scalalogic.util

import scala.collection.generic.CanBuildFrom

object SeqUtils {

  def partitionN[A, Repr <: Traversable[A], That](items: Repr, partitionFunction: A => Int, n: Option[Int] = None)(implicit bf: CanBuildFrom[Repr, A, That]): List[That] = {
    val m = items.groupBy(partitionFunction)
    val r =
      (0 until n.getOrElse(m.keys.max + 1)).map { i =>
        val b = bf(items)
        if (m.contains(i))
          for (x <- m(i))
            b += x
        b.result
      }.toList
    r
  }

}