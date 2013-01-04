package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._

trait RuleWeighter {
  def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequentAndContexts: Map[BoxerPred, Iterable[String]], vectorspace: Map[String, BowVector]): Iterable[(BoxerPred, Option[Double])]
}

case class UniformHardRuleWeighter() extends RuleWeighter {
  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequentAndContexts: Map[BoxerPred, Iterable[String]], vectorspace: Map[String, BowVector]) = {
    consequentAndContexts.map(_._1 -> None)
  }
}

case class ACtxWithCVecspaceRuleWeighter(
  compositeVectorMaker: CompositeVectorMaker)
  extends RuleWeighter {

  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequentAndContexts: Map[BoxerPred, Iterable[String]], vectorspace: Map[String, BowVector]) = {
    val pv = compositeVectorMaker.make(antecedentContext, vectorspace)
    consequentAndContexts.map {
      case (consequent, consequentContext) =>
        consequent -> Some(vectorspace.get(consequent.name) match {
          case Some(cv) => pv cosine cv
          case None => 0
        })
    }
  }
}

case class AwithCvecspaceRuleWeighter(
  compositeVectorMaker: CompositeVectorMaker)
  extends RuleWeighter {

  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequentAndContexts: Map[BoxerPred, Iterable[String]], vectorspace: Map[String, BowVector]) = {
    val pv = vectorspace.get(antecedent.name)
    consequentAndContexts.map {
      case (consequent, consequentContext) =>
        consequent -> Some(vectorspace.get(consequent.name) match {
          case Some(cv) => pv match {
    		case Some(pv) => pv cosine cv
    		case None => 0  //it is 0 not Double.NegativeInfinity
          	}
          case None => 0  //it is 0 not Double.NegativeInfinity
        })
    }
  }
}

case class AwithCtxCwithCtxVecspaceRuleWeighter(
  compositeVectorMaker: CompositeVectorMaker)
  extends RuleWeighter {

  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequentAndContexts: Map[BoxerPred, Iterable[String]], vectorspace: Map[String, BowVector]) = {
    val pv = compositeVectorMaker.make(antecedent.name +: antecedentContext.toSeq, vectorspace)
    consequentAndContexts.map {
      case (consequent, consequentContext) =>
        val cv = compositeVectorMaker.make(consequent.name +: consequentContext.toSeq, vectorspace)
        consequent -> Some(pv cosine cv)
    }
  }
}

class SameLemmaHardClauseRuleWeighter(delegate: RuleWeighter) extends RuleWeighter {
  def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequentAndContexts: Map[BoxerPred, Iterable[String]], vectorspace: Map[String, BowVector]): Iterable[(BoxerPred, Option[Double])] = {
    val (same, diff) = consequentAndContexts.partition(_._1.name == antecedent.name)
    same.map(_._1).mapToVal(Some(Double.PositiveInfinity)) ++ delegate.weightForRules(antecedent, antecedentContext, diff, vectorspace)
  }
}

case class RankingRuleWeighter(
  delegate: RuleWeighter)
  extends RuleWeighter {

  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequentAndContexts: Map[BoxerPred, Iterable[String]], vectorspace: Map[String, BowVector]) = {
    val weighted = delegate.weightForRules(antecedent, antecedentContext, consequentAndContexts, vectorspace)
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

  override def weightForRules(antecedent: BoxerPred, antecedentContext: Iterable[String], consequentAndContexts: Map[BoxerPred, Iterable[String]], vectorspace: Map[String, BowVector]) = {
    val weighted = delegate.weightForRules(antecedent, antecedentContext, consequentAndContexts, vectorspace)
    if (weighted.nonEmpty)
      Iterable(weighted.maxBy(_._2.getOrElse(Double.PositiveInfinity)))
    else
      Iterable.empty
  }
}
