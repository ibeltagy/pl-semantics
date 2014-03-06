package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import opennlp.scalabha.util.CollectionUtils._
import utcompling.mlnsemantics.vecspace._
import org.apache.commons.logging.LogFactory

trait CompositeVectorMaker {
  def make(preds: Iterable[String], vectorspace: Map[String, BowVector]): BowVector
}

case class SimpleCompositeVectorMaker() extends CompositeVectorMaker {
  private val LOG = LogFactory.getLog(classOf[SimpleCompositeVectorMaker])
  override def make(preds: Iterable[String], vectorspace: Map[String, BowVector]): BowVector = {
    val toCombine = preds.flatMap(vectorspace.get)
    LOG.info("Making phrase: " + ( preds mkString " "))
    if (toCombine.isEmpty)
      new SparseBowVector(numDims = vectorspace.head._2.size)
    else
      toCombine.reduce(_ + _)
  }
}

case class MultiplicationCompositeVectorMaker() extends CompositeVectorMaker {
  override def make(preds: Iterable[String], vectorspace: Map[String, BowVector]): BowVector = {
    preds.flatMap(vectorspace.get).reduce(_ :* _)
  }
}
