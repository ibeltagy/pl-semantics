package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import scala.collection.mutable.SetBuilder
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.mlnsemantics.wordnet.Wordnet
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerImp
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable

class InferenceRuleInjectingProbabilisticTheoremProver(
  //inferenceRuleGenerator: InferenceRuleGenerator,
  vecspaceFactory: (String => Boolean) => Map[String, BowVector],
  wordnet: Wordnet,
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  private def d(drs: BoxerExpression) = new Boxer2DrtExpressionInterpreter().interpret(drs)

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[String, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {

    assumptions.foreach(x => println(d(x.expression).pretty))
    val rules = makeNewRules(assumptions.map(_.expression), goal)
    println(d(goal).pretty)
    delegate.prove(constants, declarations, evidence, assumptions ++ rules, goal)
  }

  def makeNewRules(assumptions: List[BoxerExpression], goal: BoxerExpression): Set[WeightedExpression[BoxerExpression]] = {
    val allPreds = (assumptions :+ goal).flatMap(getAllPreds).toSet
    val rules = makeRules(allPreds)
    rules.foreach(x => println(d(x.expression).pretty))
    return rules
  }

  def getAllPreds(e: BoxerExpression): Set[BoxerPred] =
    e match {
      case p: BoxerPred => Set(p)
      case _ => e.visit(getAllPreds, (parts: List[Set[BoxerPred]]) => parts.flatten.toSet, Set.empty[BoxerPred])
    }

  private def makeRules(allPreds: Set[BoxerPred]): Set[WeightedExpression[BoxerExpression]] = {
    (for (
      (pos, preds) <- allPreds.groupBy(_.pos);
      predsByNameVar = preds.groupBy(_.name).mapValues(_.groupBy(variableType));
      pred <- preds;
      rule <- makeRulesForPred(pred, predsByNameVar)
    ) yield rule).toSet
  }

  private def makeRulesForPred(pred: BoxerPred, predsByNameVar: Map[String, Map[String, Set[BoxerPred]]]) = {
    val varType = variableType(pred)
    val synonymsAndHypernyms = getSynonyms(pred.name, pred.pos) ++ getHypernyms(pred.name, pred.pos)

    val consequents: Set[BoxerPred] =
      synonymsAndHypernyms.flatMap(nym =>
        predsByNameVar.get(nym).flatMap(constituentMap =>
          constituentMap.get(varType))).flatten.filter(_ != pred)

    makeRulesForPredConsequents(pred, consequents)
  }

  protected def makeRulesForPredConsequents(pred: BoxerPred, consequents: Set[BoxerPred]) = {
    consequents.map(consequent => makeRule(pred, consequent))
  }

  private def makeRule(antecedent: BoxerPred, consequent: BoxerPred): WeightedExpression[BoxerExpression] = {
    val BoxerPred(aDiscId, aIndices, aVariable, aName, aPos, aSense) = antecedent
    val BoxerPred(cDiscId, cIndices, cVariable, cName, cPos, cSense) = consequent
    val v = BoxerVariable(variableType(antecedent))
    val unweightedRule =
      BoxerImp(aDiscId, aIndices,
        BoxerDrs(List(List() -> v), List(BoxerPred(aDiscId, aIndices, v, aName, aPos, aSense))),
        BoxerDrs(List(), List(BoxerPred(cDiscId, cIndices, v, cName, cPos, cSense))))
    HardWeightedExpression(unweightedRule) //TODO: Apply weight here
  }

  val VariableRe = """^([a-z])\d*$""".r

  private def variableType(pred: BoxerPred): String =
    pred.variable.name match {
      case VariableRe("p") => "p"
      case VariableRe("e") => "e"
      case _ => "x"
    }

  private def getSynonyms(name: String, pos: String): Set[String] =
    (for (p <- getPos(pos); s <- wordnet.synsets(name, p); w <- s.getWords) yield w.getLemma).toSet

  private def getHypernyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- wordnet.synsets(name, p);
      h <- wordnet.allHypernyms(s);
      w <- h.getWords
    ) yield w.getLemma).toSet

  private def getPos(s: String) =
    s match {
      case "n" => List(POS.NOUN)
      case "v" => List(POS.VERB)
      case "a" => List(POS.ADJECTIVE)
      case _ => List()
    }

}
