package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.util.CollectionUtils._

trait RuleWeighter {
  def weightForRules(antecedentContext: Iterable[BoxerPred], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]): Iterable[(BoxerPred, Option[Double])]
}

case class UniformHardRuleWeighter() extends RuleWeighter {
  override def weightForRules(antecedentContext: Iterable[BoxerPred], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    consequents.mapTo(_ => None)
  }
}

case class VecspaceRuleWeighter(
  compositeVectorMaker: CompositeVectorMaker)
  extends RuleWeighter {

  override def weightForRules(antecedentContext: Iterable[BoxerPred], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    val pv = compositeVectorMaker.make(antecedentContext, vectorspace)
    consequents.mapTo { consequent =>
      Some(vectorspace.get(consequent.name) match {
        case Some(cv) => pv cosine cv
        case None => Double.NegativeInfinity
      })
    }
  }
}

case class RankingRuleWeighter(
  delegate: RuleWeighter)
  extends RuleWeighter {

  override def weightForRules(antecedentContext: Iterable[BoxerPred], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    val weighted = delegate.weightForRules(antecedentContext, consequents, vectorspace)
    val unoptionedWeighted = weighted.mapValuesStrict(_.getOrElse(Double.PositiveInfinity))
    val sortedGroupedWeighted = unoptionedWeighted.map(_.swap).groupByKey.toSeq.sortBy(-_._1).map(_._2)
    val ranked =
      sortedGroupedWeighted
        .foldLeft((Map[BoxerPred, Int](), 1)) {
          case ((accum, rank), cs) =>
            (accum ++ cs.map(_ -> rank), rank + cs.size)
        }._1
    val inverseRanked = ranked.mapValuesStrict(1. / _)
    inverseRanked.normalizeValues.mapValuesStrict(Option(_))
  }
}

case class TopRuleWeighter(
  delegate: RuleWeighter)
  extends RuleWeighter {

  override def weightForRules(antecedentContext: Iterable[BoxerPred], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    val weighted = delegate.weightForRules(antecedentContext, consequents, vectorspace)
    if (weighted.nonEmpty)
      Iterable(weighted.maxBy(_._2.getOrElse(Double.PositiveInfinity)))
    else
      Iterable.empty
  }
}
