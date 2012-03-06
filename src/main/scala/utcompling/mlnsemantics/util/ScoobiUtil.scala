package utcompling.mlnsemantics.util

import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.DList
import com.nicta.scoobi.DList._
import com.nicta.scoobi.io.text.TextInput._
import com.nicta.scoobi.io.text.TextOutput._
import com.nicta.scoobi.Job
import utcompling.mlnsemantics.util.HadoopUtils._

object ScoobiUtil {

  class DList_with_toIterable[T: Manifest](dlist: DList[T]) {
    def toIterable(): Iterable[T] = {
      val materialized = dlist.materialize
      val job = Job()
      job << DList.use(materialized)
      job.run()
      materialized.get
    }

    def materializeGet(conv: String => T): T = {
      val tempFile = "scoobi-toIterableHack-temp-" + System.currentTimeMillis
      persist(toTextFile(dlist, tempFile))
      val result = getMerge(tempFile).trim
      try {
        conv(result)
      } catch {
        case _ => throw new RuntimeException("could not convert: " + result)
      }
    }
  }
  implicit def DList_with_toIterable[T: Manifest](dlist: DList[T]) = new DList_with_toIterable(dlist)

}
