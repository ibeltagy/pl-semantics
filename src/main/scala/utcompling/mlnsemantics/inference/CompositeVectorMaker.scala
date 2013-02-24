package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import opennlp.scalabha.util.CollectionUtils._
import utcompling.mlnsemantics.vecspace.BowVectorSpace

trait CompositeVectorMaker {
  def make(preds: Iterable[String], vectorspace: Map[String, BowVector]): BowVector
}

case class SimpleCompositeVectorMaker() extends CompositeVectorMaker {
  override def make(preds: Iterable[String], vectorspace: Map[String, BowVector]): BowVector = {
    preds.flatMap(vectorspace.get).fold(new BowVector(Map()))(_ + _)
  }
}

case class MultiplicationCompositeVectorMaker() extends CompositeVectorMaker {
  override def make(preds: Iterable[String], vectorspace: Map[String, BowVector]): BowVector = {
    var vectors = preds.flatMap(vectorspace.get)
    vectors.size match {
      case 0 => new BowVector(Map());
      case 1 => vectors.head
      case _ => vectors = vectors.slice(1, vectors.size);
      			vectors.fold(vectors.head)(_ * _); 
    }
  }
}