package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import opennlp.scalabha.util.CollectionUtils._
import utcompling.mlnsemantics.vecspace._
import utcompling.mlnsemantics.run.Sts

trait CompositeVectorMaker {
  def make(preds: Iterable[String], vectorspace: Map[String, BowVector]): BowVector
}

case class SimpleCompositeVectorMaker() extends CompositeVectorMaker {
  override def make(preds: Iterable[String], vectorspace: Map[String, BowVector]): BowVector = {
    /*
    println(Sts.text)
    println(Sts.textLemma)
    println(Sts.hypothesis)
    println(Sts.hypothesisLemma)
    */    
    
    val toCombine = preds.flatMap(vectorspace.get)
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

