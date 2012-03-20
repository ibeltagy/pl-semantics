package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions.asScalaBuffer
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

    val rules = makeNewRules(assumptions.map(_.expression), goal).map(HardWeightedExpression(_))
    delegate.prove(constants, declarations, evidence, assumptions ++ rules, goal)

  }

  def makeNewRules(assumptions: List[BoxerExpression], goal: BoxerExpression): Set[BoxerExpression] = {
    val allPreds = (assumptions :+ goal).flatMap(getAllPreds).toSet
    val rules = makeRules(allPreds)
    rules.foreach(x => println(d(x).pretty))
    return rules
  }

  def getAllPreds(e: BoxerExpression): Set[BoxerPred] =
    e match {
      case p: BoxerPred => Set(p)
      case _ => e.visit(getAllPreds, (parts: List[Set[BoxerPred]]) => parts.flatten.toSet, Set())
    }

  private def makeRules(allPreds: Set[BoxerPred]): Set[BoxerExpression] = {
    val rules = new SetBuilder[BoxerExpression, Set[BoxerExpression]](Set[BoxerExpression]())
    for ((pos, preds) <- allPreds.groupBy(_.pos)) {
      val predsByNameVar = preds.groupBy(_.name).mapValues(_.groupBy(variableType))
      for (pred <- preds) {
        val varType = variableType(pred)
        for (nym <- (getSynonyms(pred.name, pred.pos) ++ getHypernyms(pred.name, pred.pos))) {
          for (consequent <- predsByNameVar.getOrElse(nym, Map()).getOrElse(varType, Set())) {
            if (pred != consequent)
              rules += makeRule(pred, consequent)
          }
        }
      }
    }
    return rules.result
  }

  private def makeRule(antecedent: BoxerPred, consequent: BoxerPred): BoxerExpression = {
    val BoxerPred(aDiscId, aIndices, aVariable, aName, aPos, aSense) = antecedent
    val BoxerPred(cDiscId, cIndices, cVariable, cName, cPos, cSense) = consequent
    val v = BoxerVariable(variableType(antecedent))
    return BoxerImp(aDiscId, aIndices,
      BoxerDrs(List(List() -> v), List(BoxerPred(aDiscId, aIndices, v, aName, aPos, aSense))),
      BoxerDrs(List(), List(BoxerPred(cDiscId, cIndices, v, cName, cPos, cSense))))
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
      h <- wordnet.hypernyms(s);
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
