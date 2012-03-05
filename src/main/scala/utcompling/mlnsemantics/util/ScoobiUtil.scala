package utcompling.mlnsemantics.util

import com.nicta.scoobi.DList
import com.nicta.scoobi.Job

object ScoobiUtil {

  class DList_with_toIterable[T](dlist: DList[T]) {
    def toIterable(): Iterable[T] = {
      val materialized = dlist.materialize
      val job = Job()
      job << DList.use(materialized)
      job.run()
      materialized.get
    }
  }
  implicit def DList_with_toIterable[T](dlist: DList[T]) = new DList_with_toIterable(dlist)

}
