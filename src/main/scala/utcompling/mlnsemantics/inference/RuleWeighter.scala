package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._

trait RuleWeighter {
  def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]): Iterable[(BoxerPred, Option[Double])]
}

case class UniformHardRuleWeighter() extends RuleWeighter {
  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    consequents.map(_ -> None)
  }
}

case class VecspaceRuleWeighter(
  compositeVectorMaker: CompositeVectorMaker)
  extends RuleWeighter {

  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    val pv = compositeVectorMaker.make(antecedentContext, vectorspace)
    consequents.mapTo { consequent =>
      Some(vectorspace.get(consequent.name) match {
        case Some(cv) => pv cosine cv
        case None => Double.NegativeInfinity
      })
    }
  }
}

class SameLemmaHardClauseRuleWeighter(delegate: RuleWeighter) extends RuleWeighter {
  def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]): Iterable[(BoxerPred, Option[Double])] = {
    val (same, diff) = consequents.partition(_.name == antecedent.name)
    same.mapToVal(Some(Double.PositiveInfinity)) ++ delegate.weightForRules(antecedent, antecedentContext, diff, vectorspace)
  }
}

case class RankingRuleWeighter(
  delegate: RuleWeighter)
  extends RuleWeighter {

  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    val weighted = delegate.weightForRules(antecedent, antecedentContext, consequents, vectorspace)
    val unoptionedWeighted = weighted.mapVals(_.getOrElse(Double.PositiveInfinity))
    val sortedGroupedWeighted = unoptionedWeighted.map(_.swap).groupByKey.toSeq.sortBy(-_._1).map(_._2)
    val ranked =
      sortedGroupedWeighted
        .foldLeft((Map[BoxerPred, Int](), 1)) {
          case ((accum, rank), cs) =>
            (accum ++ cs.map(_ -> rank), rank + cs.size)
        }._1
    val inverseRanked = ranked.mapVals(1. / _)
    inverseRanked.normalizeValues.mapVals(Option(_))
  }
}

case class TopRuleWeighter(
  delegate: RuleWeighter)
  extends RuleWeighter {

  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequents: Set[BoxerPred], vectorspace: Map[String, BowVector]) = {
    val weighted = delegate.weightForRules(antecedent, antecedentContext, consequents, vectorspace)
    if (weighted.nonEmpty)
      Iterable(weighted.maxBy(_._2.getOrElse(Double.PositiveInfinity)))
    else
      Iterable.empty
  }
}
