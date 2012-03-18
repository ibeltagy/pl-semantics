package utcompling.mlnsemantics.modal

import scala.collection.mutable.SetBuilder
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.inference.TheoremProver
import utcompling.scalalogic.util.StringUtils._
import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerImp
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable
import utcompling.mlnsemantics.wordnet.Wordnet
import utcompling.mlnsemantics.wordnet.WordnetImpl

class WordnetModalTheoremProverDecorator[R](
  theoremProver: TheoremProver[BoxerExpression, R],
  wn: Wordnet = new WordnetImpl())
  extends TheoremProver[BoxerExpression, R] {

  private def d(drs: BoxerExpression) = new Boxer2DrtExpressionInterpreter().interpret(drs)

  def prove(assumptions: List[BoxerExpression], goal: Option[BoxerExpression], verbose: Boolean = false): Option[R] = {
    val newRules = makeNewRules(assumptions, goal)
    return theoremProver.prove(assumptions ++ newRules, goal, verbose)
  }

  def makeNewRules(assumptions: List[BoxerExpression], goal: Option[BoxerExpression], verbose: Boolean = false): Set[BoxerExpression] = {
    val allPreds = (assumptions ++ (goal.map(List(_)).getOrElse(List()))).flatMap(getAllPreds).toSet
    val rules = makeRules(allPreds)
    rules.map(x => println(d(x).pretty))
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

  private def variableType(pred: BoxerPred): String =
    """^([a-z])\d*$""".r.findFirstMatchIn(pred.variable.name).get.group(1) match {
      case "p" => "p"
      case "e" => "e"
      case _ => "x"
    }

  private def getSynonyms(name: String, pos: String): Set[String] =
    (for (p <- getPos(pos); s <- wn.synsets(name, p); w <- s.getWords) yield w.getLemma).toSet

  private def getHypernyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- wn.synsets(name, p);
      h <- wn.hypernyms(s);
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
