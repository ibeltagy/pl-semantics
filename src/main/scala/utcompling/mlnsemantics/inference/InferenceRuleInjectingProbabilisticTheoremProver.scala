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
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerEqv
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import org.apache.commons.logging.LogFactory
import support.HardWeightedExpression

class InferenceRuleInjectingProbabilisticTheoremProver(
  wordnet: Wordnet,
  vecspaceFactory: ((String => Boolean) => Map[String, BowVector]),
  ruleWeighter: RuleWeighter,
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  private val LOG = LogFactory.getLog(classOf[InferenceRuleInjectingProbabilisticTheoremProver])

  private val NotPred = """^not_(.+)$""".r

  private def d(drs: BoxerExpression) =
    new Boxer2DrtExpressionInterpreter().interpret(
      new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(drs))

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {

    assumptions.foreach(x => LOG.info("\n" + d(x.expression).pretty))
    val rules = makeNewRules(assumptions.map(_.expression), goal)
    LOG.info("\n" + d(goal).pretty)
    delegate.prove(constants, declarations, evidence, assumptions ++ rules, goal)
  }

  def makeNewRules(assumptions: List[BoxerExpression], goal: BoxerExpression): Set[WeightedExpression[BoxerExpression]] = {
    val assumPredsAndContexts = assumptions.flatMap(getAllPredsAndContexts)
    val goalPredsAndContexts = (List(goal)).flatMap(getAllPredsAndContexts)
    val allPredsAndContexts =  List.concat(assumPredsAndContexts, goalPredsAndContexts);
    val vectorspace = vecspaceFactory(allPredsAndContexts.flatMap { case (pred, context) => (pred.name +: context).map(stripNot) }.toSet)
    val rules = makeRules(assumPredsAndContexts, goalPredsAndContexts, vectorspace)
    //rules.foreach(x => LOG.info("\n" + d(x.expression).pretty))
    return rules
  }

  def getAllPredsAndContexts(e: BoxerExpression): Seq[(BoxerPred, Seq[String])] = {
    val preds = getAllPreds(e)
    val predsAndContexts = preds.zipWithIndex.map { case (p, i) => p -> (preds.take(i) ++ preds.drop(i + 1)) }
    predsAndContexts.mapVals(_.map(p => stripNot(p.name)))
  }

  private def getAllPreds(e: BoxerExpression): Seq[BoxerPred] =
    e match {
      case p: BoxerPred => Seq(p)
      case _ => e.visit(getAllPreds, (parts: List[Seq[BoxerPred]]) => parts.flatten, Seq.empty[BoxerPred])
    }

  private def makeRules(assumPredsAndContexts: Iterable[(BoxerPred, Iterable[String])], goalPredsAndContexts: Iterable[(BoxerPred, Iterable[String])], vectorspace: Map[String, BowVector]): Set[WeightedExpression[BoxerExpression]] = {
    
    var ret = List[WeightedExpression[BoxerExpression]] () ;
	for (assumPred <- assumPredsAndContexts)
	{
		for (goalPred <- goalPredsAndContexts)
		{
			if (assumPred._1.pos == goalPred._1.pos)
			{
				var pred = assumPred._1;
				var antecedentContext = assumPred._2;
				var rhs = Map(goalPred._1 -> goalPred._2);
				var predsAndContextsByName = Map(goalPred._1.name -> rhs); 
				var rule = makeRulesForPred(pred, antecedentContext, predsAndContextsByName, vectorspace);
				ret = List.concat(ret, rule);
			}
		}
	} 
	return ret.toSet;
  }

  private def stripNot(p: String) = p match { case NotPred(x) => x; case x => x }

  private def makeRulesForPred(pred: BoxerPred, antecedentContext: Iterable[String], predsAndContextsByName: Map[String, Map[BoxerPred, Iterable[String]]], vectorspace: Map[String, BowVector]) = {
    pred.name match {
      case NotPred(_) =>
        makeRulesForNegPred(pred, antecedentContext, predsAndContextsByName, vectorspace)
      case _ =>
        makeRulesForPosPred(pred, antecedentContext, predsAndContextsByName, vectorspace)
    }
  }

  private def makeRulesForPosPred(pred: BoxerPred, antecedentContext: Iterable[String], predsAndContextsByName: Map[String, Map[BoxerPred, Iterable[String]]], vectorspace: Map[String, BowVector]) = {
   //val synonymsAndHypernyms = getSynonyms(pred.name, pred.pos) ++ getHypernyms(pred.name, pred.pos)   //skip checking wordnet
    val synonymsAndHypernyms = predsAndContextsByName.keySet
    val consequentAndContexts = synonymsAndHypernyms.flatMap(predsAndContextsByName.get).flatten.filter(_._1 != pred)
    	.filter(_._1.name match { case NotPred(_) => false; case _ => true })
    for ((consequent, weight) <- ruleWeighter.weightForRules(pred, antecedentContext, consequentAndContexts.toMap, vectorspace))
      yield makeRule(pred, consequent, weight)
  }

  private def makeRulesForNegPred(pred: BoxerPred, antecedentContext: Iterable[String], predsAndContextsByName: Map[String, Map[BoxerPred, Iterable[String]]], vectorspace: Map[String, BowVector]) = {
    val NotPred(simplePredName) = pred.name
    //val synonymsAndHypernyms = getSynonyms(simplePredName, pred.pos) ++ getHyponyms(simplePredName, pred.pos) //skip checking wordnet
    val synonymsAndHypernyms = predsAndContextsByName.keySet
    val consequentAndContexts = synonymsAndHypernyms.flatMap(predsAndContextsByName.get).flatten.filter(_._1 != pred)
      .filter(_._1.name match { case NotPred(_) => true; case _ => false })
    for ((consequent, weight) <- ruleWeighter.weightForRules(pred, antecedentContext, consequentAndContexts.toMap, vectorspace))
      yield makeRule(pred, consequent, weight)
  }

  private def makeRule(antecedent: BoxerPred, consequent: BoxerPred, weight: Option[Double]): WeightedExpression[BoxerExpression] = {
    val BoxerPred(aDiscId, aIndices, aVariable, aName, aPos, aSense) = antecedent
    val BoxerPred(cDiscId, cIndices, cVariable, cName, cPos, cSense) = consequent
    val v = BoxerVariable(variableType(antecedent))
    val unweightedRule =
      BoxerEqv(aDiscId, aIndices,
        BoxerDrs(List(Nil -> v), List(BoxerPred(aDiscId, aIndices, v, aName, aPos, aSense))),
        BoxerDrs(Nil, List(BoxerPred(cDiscId, cIndices, v, cName, cPos, cSense))))
    weight match {
      case Some(w) => SoftWeightedExpression(unweightedRule, w)
      case None => HardWeightedExpression(unweightedRule)
    }
  }

  val VariableRe = """^([a-z])\d*$""".r

  private def variableType(pred: BoxerPred): String =
    pred.variable.name match {
      case VariableRe("p") => "p"
      case VariableRe("e") => "e"
      case _ => "x"
    }

  private def getSynonyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- wordnet.synsets(name, p);
      w <- s.getWords
    ) yield w.getLemma).toSet + name -- Set("POS", "NEG") //TODO: REMOVE THE "+ name".  WE ONLY WANT NEED THIS FOR WHEN THE WORD ISN'T IN WORDNET.

  private def getHypernyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- wordnet.synsets(name, p);
      h <- wordnet.allHypernyms(s, 20);
      w <- h.getWords
    ) yield w.getLemma).toSet

  private def getHyponyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- wordnet.synsets(name, p);
      h <- wordnet.allHyponyms(s, 20);
      w <- h.getWords
    ) yield w.getLemma).toSet

  private def getPos(s: String) =
    s match {
      case "n" => List(POS.NOUN)
      case "v" => List(POS.VERB)
      case "a" => List(POS.ADJECTIVE)
      case _ => Nil
    }

}
