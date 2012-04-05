package utcompling.mlnsemantics.inference

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.util.CollectionUtils._

trait RuleWeighter {
  def weightForRules(pred: BoxerPred, consequents: Iterable[BoxerPred]): Iterable[(BoxerPred, Option[Double])]
}

case class UniformHardRuleWeighter() extends RuleWeighter {
  override def weightForRules(pred: BoxerPred, consequents: Iterable[BoxerPred]) = {
    consequents.mapTo(_ => None)
  }
}

case class VecspaceRuleWeighter(
  vecspaceFactory: (String => Boolean) => Map[String, BowVector])
  extends RuleWeighter {

  override def weightForRules(pred: BoxerPred, consequents: Iterable[BoxerPred]) = {
    consequents.toSeq match {
      case Seq() =>
        Iterable.empty
      case Seq(consequent) =>
        Iterable(consequent -> Some(1.))
      case Seq(consequents @ _*) =>
        val p = pred.name
        val vs = vecspaceFactory(consequents.map(_.name).toSet + p)
        vs.get(p) match {
          case Some(pv) =>
            val similarities =
              consequents.mapTo { consequent =>
                vs.get(consequent.name) match {
                  case Some(cv) => pv cosine cv
                  case None => Double.NegativeInfinity
                }
              }
            val ranks = similarities.sortBy(_._2).zipWithIndex.map { case ((c, _), rank) => (c, rank) }
            ranks.normalizeValues.mapValuesStrict(Option(_))
          case None =>
            Iterable.empty
        }
    }
  }
}

case class TopRuleWeighter(
  delegate: RuleWeighter)
  extends RuleWeighter {

  override def weightForRules(pred: BoxerPred, consequents: Iterable[BoxerPred]) = {
    val weighted = delegate.weightForRules(pred, consequents)
    if (weighted.nonEmpty) {
      val topConsequent = weighted.maxBy(_._2.getOrElse(Double.PositiveInfinity))._1
      Iterable(topConsequent -> None)
    }
    else {
      Iterable.empty
    }
  }
}
