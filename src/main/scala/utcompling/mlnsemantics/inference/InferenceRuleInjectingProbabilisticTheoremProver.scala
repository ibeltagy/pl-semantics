package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.{BowVector, BowVectorSpace}
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
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import utcompling.mlnsemantics.rules.DistributionalRules
import utcompling.mlnsemantics.rules.ParaphraseRules
import utcompling.mlnsemantics.rules.Rules
import utcompling.mlnsemantics.rules.OnTheFlyRules
import utcompling.mlnsemantics.rules.WordNetRules
import utcompling.mlnsemantics.rules.PhrasalRules
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerCard
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerTimex
import utcompling.mlnsemantics.rules.DiffRules

class InferenceRuleInjectingProbabilisticTheoremProver(

  vecspaceFactory: ((String => Boolean) => BowVectorSpace),
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
    goal: BoxerExpression): Seq[Double] = {
    
    assumptions.foreach(x => LOG.info("\n" + d(x.expression).pretty))
    LOG.info("\n" + d(goal).pretty)
/*
    var preds = assumptions.head.expression.getPredicates
    var rels = assumptions.head.expression.getRelations
    var simplePh = PhrasalRules.findSimplePhrases(preds);
    var relationalPh = PhrasalRules.findRelationalPhrases(rels, simplePh);
    var nounPh = PhrasalRules.findNounPhrases(simplePh);
    var prepPh = PhrasalRules.findPrepPhrase(simplePh, relationalPh);
    var verbPh = PhrasalRules.findVerbPhrase(simplePh, relationalPh);

    LOG.trace("Assumption: ----------------")        
    LOG.trace("Preds: ")
    preds.sortBy(_.variable).foreach(LOG.trace(_))
    LOG.trace("Rels: ")
    rels.sortBy(_.variable).foreach(LOG.trace(_))
    LOG.trace("SimplePhrase: ")
    simplePh.foreach(LOG.trace(_))
    LOG.trace("RelationalPhrase: ")
    relationalPh.foreach(LOG.trace(_))
    LOG.trace("NounPhrase: ")
    nounPh.foreach(LOG.trace(_))
    LOG.trace("PrepPhrase: ")
    prepPh.foreach(LOG.trace(_))    
    LOG.trace("VerbPhrase: ")
    verbPh.foreach(LOG.trace(_))    

    LOG.trace("Goal: ----------------")
    preds = goal.getPredicates
    rels = goal.getRelations
    simplePh = PhrasalRules.findSimplePhrases(preds);
    relationalPh = PhrasalRules.findRelationalPhrases(rels, simplePh);
    nounPh = PhrasalRules.findNounPhrases(simplePh);
    prepPh = PhrasalRules.findPrepPhrase(simplePh, relationalPh);
    verbPh = PhrasalRules.findVerbPhrase(simplePh, relationalPh);
    
    LOG.trace("Preds: ")
    preds.sortBy(_.variable).foreach(LOG.trace(_))
    LOG.trace("Rels: ")
    rels.sortBy(_.variable).foreach(LOG.trace(_))
    LOG.trace("SimplePhrase: ")
    simplePh.foreach(LOG.trace(_))
    LOG.trace("RelationalPhrase: ")
    relationalPh.foreach(LOG.trace(_))
    LOG.trace("NounPhrase: ")
    nounPh.foreach(LOG.trace(_))
    LOG.trace("PrepPhrase: ")
    prepPh.foreach(LOG.trace(_))    
    LOG.trace("VerbPhrase: ")
    verbPh.foreach(LOG.trace(_))    
        
*/
    val simplifiedDeclarations = declarations.map( d => {
		val name = d._1 match {
			case BoxerPred(discId, indices, variable, name, pos, sense) => name
			case BoxerRel(discId, indices, event, variable, name, sense) => name
			case BoxerNamed(discId, indices, variable, name, typ, sense) => name
			case BoxerCard(discId, indices, variable, num, typ) => "card_" + num
			case BoxerTimex(discId, indices, variable, timeExp) => "time"
		}
		name -> d._2
	} )
    //query lucene for pre-compiled distributional rules
	val distributionalRules = new DistributionalRules().getRules();

	//query lucene for pre-compiled paraphrase rules
	val paraphraseRules = new ParaphraseRules().getRules();
		
	val precompiledRules = Rules.convertRulesToFOL(distributionalRules ++  paraphraseRules, assumptions.head.expression, goal)
								.map(r=> (r._1, r._2, r._3 * Sts.opts.rulesWeight, r._4)) //scale weights of all precompiles rules
	
	    //generate distributional inference rules on the fly
	val onthefulyRules = new OnTheFlyRules().getRules(assumptions.head.expression, goal, ruleWeighter, vecspaceFactory)
										.map(r=> (r._1, r._2, r._3 * Sts.opts.distWeight, r._4)) //scale weights of all onthefly rules
										
          //Hard rules from WordNet
	val wordNetRules = new WordNetRules().getRules(assumptions.head.expression, goal, simplifiedDeclarations);
	
	val diffRule = new DiffRules().getRule(assumptions.head.expression, goal);
    
    val rules:List[WeightedExpression[BoxerExpression]] = Sts.opts.inferenceRulesLevel match {
		case -1 => List();
		case _ => (precompiledRules ++  onthefulyRules ++ wordNetRules ++ diffRule).flatMap(r=>Rules.createWeightedExpression(r._1, r._2, r._3, r._4, simplifiedDeclarations));
	 } 
  
    delegate.prove(constants, declarations, evidence, assumptions ++ (rules.toSet.toList), goal)
  }
}
