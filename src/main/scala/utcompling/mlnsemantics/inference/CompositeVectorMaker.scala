package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import opennlp.scalabha.util.CollectionUtils._
import utcompling.mlnsemantics.vecspace._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.run.Sts

trait CompositeVectorMaker {
  def make(preds: Iterable[String], vectorspace: BowVectorSpace): BowVector
}

case class SimpleCompositeVectorMaker() extends CompositeVectorMaker {
  private val LOG = LogFactory.getLog(classOf[SimpleCompositeVectorMaker])
  override def make(preds: Iterable[String], vectorspace: BowVectorSpace): BowVector = {
    LOG.info("Making phrase: " + ( preds mkString " "))
    /*
    println(Sts.text)
    println(Sts.textLemma)
    println(Sts.hypothesis)
    println(Sts.hypothesisLemma)
    */
    val toCombine = preds.map(_.split("-").head).flatMap(vectorspace.get)
    if (toCombine.isEmpty)
      new SparseBowVector(numDims = vectorspace.numDims)
    else
      toCombine.reduce(_ + _)
  }
}

case class MultiplicationCompositeVectorMaker() extends CompositeVectorMaker {
  override def make(preds: Iterable[String], vectorspace: BowVectorSpace): BowVector = {
    preds.map(_.split("-").head).flatMap(vectorspace.get).reduce(_ :* _)
  }
}

