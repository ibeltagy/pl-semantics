package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerImp
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerEqv
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import opennlp.scalabha.util.CollectionUtil._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import org.apache.commons.logging.LogFactory
import support.HardWeightedExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerNamed
import utcompling.mlnsemantics.run.Sts
import dhg.depparse.Lemmatize
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import utcompling.mlnsemantics.rules.DistributionalRules
import utcompling.mlnsemantics.rules.ParaphraseRules
import utcompling.mlnsemantics.rules.Rules
import utcompling.mlnsemantics.rules.OnTheFlyRules

class InferenceRuleInjectingProbabilisticTheoremProver(

  vecspaceFactory: ((String => Boolean) => Map[String, BowVector]),
  ruleWeighter: RuleWeighter,
 
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  private val LOG = LogFactory.getLog(classOf[InferenceRuleInjectingProbabilisticTheoremProver])
  
  private val distWeight = Sts.opts.distWeight;
  private val resourceWeight = Sts.opts.rulesWeight;

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
    LOG.info("\n" + d(goal).pretty)

    //query lucene for pre-compiled distributional rules
	val distributionalRules = new DistributionalRules().getRules();

	//query lucene for pre-compiled paraphrase rules
	val paraphraseRules = new ParaphraseRules().getRules();
		
	val precompiledRules = Rules.convertRulesToFOL(distributionalRules ++  paraphraseRules, assumptions.head.expression, goal)
								.map(r=> (r._1, r._2, r._3 * Sts.opts.rulesWeight)) //scale weights of all precompiles rules
	
	    //generate distributional inference rules on the fly
	val onthefulyRules = new OnTheFlyRules().getRules(assumptions.head.expression, goal, ruleWeighter, vecspaceFactory)
										.map(r=> (r._1, r._2, r._3 * Sts.opts.distWeight)) //scale weights of all onthefly rules
    
    val rules:List[WeightedExpression[BoxerExpression]] = Sts.opts.inferenceRulesLevel match {
		case -1 => List();
		case _ => (precompiledRules ++  onthefulyRules).flatMap(r=>Rules.createWeightedExpression(r._1, r._2, r._3));
	 } 
  
    delegate.prove(constants, declarations, evidence, assumptions ++ (rules.toSet.toList), goal)
  }
}
