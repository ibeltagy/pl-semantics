package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import opennlp.scalabha.util.CollectionUtils._
import utcompling.mlnsemantics.vecspace._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.run.Sts

trait CompositeVectorMaker {
  def make(preds: Iterable[String], vectorspace: Map[String, BowVector], sentence: String, lemmatizedSent: String): BowVector
}

case class SimpleCompositeVectorMaker() extends CompositeVectorMaker {
  private val LOG = LogFactory.getLog(classOf[SimpleCompositeVectorMaker])
  override def make(preds: Iterable[String], vectorspace: Map[String, BowVector], sentence: String, lemmatizedSent: String): BowVector = {
    LOG.info("Making phrase: " + ( preds mkString " "))
    LOG.info("Sentence: '" + sentence + "' / '" + lemmatizedSent)
    val toCombine = preds.map(_.split("-").head).flatMap(vectorspace.get)
    if (toCombine.isEmpty)
      new SparseBowVector(numDims = vectorspace.head._2.size)
    else
      toCombine.reduce(_ + _)
  }
}

case class MultiplicationCompositeVectorMaker() extends CompositeVectorMaker {
  override def make(preds: Iterable[String], vectorspace: Map[String, BowVector], sentence: String, lemmatizedSent: String): BowVector = {
    preds.map(_.split("-").head).flatMap(vectorspace.get).reduce(_ :* _)
  }
}

