package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.util.CollectionUtils._

trait RuleWeighter {
  def weightForRules(antecedentPreds: Iterable[BoxerPred], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]): Iterable[(BoxerPred, Option[Double])]
}

case class UniformHardRuleWeighter() extends RuleWeighter {
  override def weightForRules(antecedentPreds: Iterable[BoxerPred], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    consequents.mapTo(_ => None)
  }
}

case class VecspaceRuleWeighter(
  compositeVectorMaker: CompositeVectorMaker)
  extends RuleWeighter {

  override def weightForRules(antecedentPreds: Iterable[BoxerPred], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    // TODO: Remove this match stuff?
    consequents.toSeq match {
      case Seq() => 
        Iterable.empty
      case Seq(consequent) =>
        Iterable(consequent -> Some(1.))
      case Seq(consequents @ _*) =>
        val pv = compositeVectorMaker.make(antecedentPreds, vectorspace)
        val similarities =
          consequents.map { consequent =>
            (vectorspace.get(consequent.name) match {
              case Some(cv) => pv cosine cv
              case None => Double.NegativeInfinity
            }, consequent)
          }
        val sortedGroupedSimilarities = similarities.groupByKey.toSeq.sortBy(-_._1).map(_._2)
        val ranks =
          sortedGroupedSimilarities
            .foldLeft((Map[BoxerPred, Int](), 1)) {
              case ((accum, rank), cs) =>
                (accum ++ cs.mapTo(_ => rank), rank + cs.size)
            }._1
        ranks.normalizeValues.mapValuesStrict(Option(_))
    }
  }
}

case class TopRuleWeighter(
  delegate: RuleWeighter)
  extends RuleWeighter {

  override def weightForRules(antecedentPreds: Iterable[BoxerPred], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    val weighted = delegate.weightForRules(antecedentPreds, consequents, vectorspace)
    if (weighted.nonEmpty) {
      val topConsequent = weighted.maxBy(_._2.getOrElse(Double.PositiveInfinity))._1
      Iterable(topConsequent -> None)
    }
    else {
      Iterable.empty
    }
  }
}
