package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.util.CollectionUtils._
import utcompling.mlnsemantics.vecspace.BowVectorSpace

trait CompositeVectorMaker {
  def make(preds: Iterable[BoxerPred], vectorspace: Map[String, BowVector]): BowVector
}

case class SimpleCompositeVectorMaker() extends CompositeVectorMaker {
  override def make(preds: Iterable[BoxerPred], vectorspace: Map[String, BowVector]): BowVector = {
    preds.flatMap(p => vectorspace.get(p.name)).fold(new BowVector(Map()))(_ + _)
  }
}
