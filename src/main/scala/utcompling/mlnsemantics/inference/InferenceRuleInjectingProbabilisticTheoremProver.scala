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

    if (Sts.opts.genPhrases)
    {
	    var tpreds = assumptions.head.expression.getPredicates
	    var trels = assumptions.head.expression.getRelations
	    var tsimplePh = PhrasalRules.findSimplePhrases(tpreds);
	    var trelationalPh = PhrasalRules.findRelationalPhrases(trels, tsimplePh);
	    var tnounPh = PhrasalRules.findNounPhrases(tsimplePh);
	    var tprepPh = PhrasalRules.findPrepPhrase(tsimplePh, trelationalPh);
	    var tverbPh = PhrasalRules.findVerbPhrase(tsimplePh, trelationalPh);
	
	    LOG.trace("Assumption: ----------------")        
	    LOG.trace("Preds: ")
	    tpreds.sortBy(_.variable).foreach(LOG.trace(_))
	    LOG.trace("Rels: ")
	    trels.sortBy(_.variable).foreach(LOG.trace(_))
	    LOG.trace("SimplePhrase: ")
	    tsimplePh.foreach(LOG.trace(_))
	    LOG.trace("RelationalPhrase: ")
	    trelationalPh.foreach(LOG.trace(_))
	    LOG.trace("NounPhrase: ")
	    tnounPh.foreach(LOG.trace(_))
	    LOG.trace("PrepPhrase: ")
	    tprepPh.foreach(LOG.trace(_))    
	    LOG.trace("VerbPhrase: ")
	    tverbPh.foreach(LOG.trace(_))    
	
	    LOG.trace("Goal: ----------------")
	    var hpreds = goal.getPredicates
	    var hrels = goal.getRelations
	    var hsimplePh = PhrasalRules.findSimplePhrases(hpreds);
	    var hrelationalPh = PhrasalRules.findRelationalPhrases(hrels, hsimplePh);
	    var hnounPh = PhrasalRules.findNounPhrases(hsimplePh);
	    var hprepPh = PhrasalRules.findPrepPhrase(hsimplePh, hrelationalPh);
	    var hverbPh = PhrasalRules.findVerbPhrase(hsimplePh, hrelationalPh);
	    
	    LOG.trace("Preds: ")
	    hpreds.sortBy(_.variable).foreach(LOG.trace(_))
	    LOG.trace("Rels: ")
	    hrels.sortBy(_.variable).foreach(LOG.trace(_))
	    LOG.trace("SimplePhrase: ")
	    hsimplePh.foreach(LOG.trace(_))
	    LOG.trace("RelationalPhrase: ")
	    hrelationalPh.foreach(LOG.trace(_))
	    LOG.trace("NounPhrase: ")
	    hnounPh.foreach(LOG.trace(_))
	    LOG.trace("PrepPhrase: ")
	    hprepPh.foreach(LOG.trace(_))    
	    LOG.trace("VerbPhrase: ")
	    hverbPh.foreach(LOG.trace(_))   
	
	    //Templates: 
	    //1) NounPhrase <=> NounPhrase
	    //2) PrepPhrase <=> PrepPhrase
	    //3) PrepPhrase <=> NounPhrase  //match NounPhrase variable with Head of PrepPhrase variable
	    //4) VerbPhrase <=> VerbPhrase
	    
	    var tphrases = tnounPh ++ tprepPh ++ tverbPh
	    var hphrases = hnounPh ++ hprepPh ++ hverbPh
	    
	    tphrases.foreach(tPhrase => {
			hphrases.foreach(hPhrase => {
				if (PhrasalRules.isCompatible(tPhrase, hPhrase))
				{
					for( i <- 0 until tPhrase.size)
					{
						 for( j <- 0 until hPhrase.size)
						 {
							 val lhs = PhrasalRules.ruleSideToString(tPhrase.getBoxerExpressionList(i), Sts.text, false)
							 val rhs = PhrasalRules.ruleSideToString(hPhrase.getBoxerExpressionList(i), Sts.hypothesis, false)
							 if (lhs == "" || rhs == "")
								 println("No DIST rule for: " + tPhrase.getBoxerExpressionList(i) + "=>" +  hPhrase.getBoxerExpressionList(i))
							 else
								 println ("[X]\t" + lhs + "\t" + rhs+ "\t" + "0.5" + "\t" +Sts.text + "\t" + Sts.hypothesis)
						 }
					}
				}
			})
	    })
    }
    
    


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
		
	val precompiledRules = Rules.convertRulesToFOL( /*distributionalRules ++ */ paraphraseRules, assumptions.head.expression, goal)
								.map(r=> (r._1, r._2, r._3 * Sts.opts.rulesWeight, r._4)) //scale weights of all precompiles rules
	
	    //generate distributional inference rules on the fly
	val onthefulyRules = new OnTheFlyRules().getRules(assumptions.head.expression, goal, ruleWeighter, vecspaceFactory)
										.map(r=> (r._1, r._2, r._3 * Sts.opts.distWeight, r._4)) //scale weights of all onthefly rules
										
          //Hard rules from WordNet
	val wordNetRules = new WordNetRules().getRules(assumptions.head.expression, goal, simplifiedDeclarations);
	
	val diffRule = if (Sts.opts.extendDiffRulesLvl.isDefined)
	{
	  //Generate rules for whatever current level
	  new DiffRules().getRule(assumptions.head.expression, goal, ruleWeighter, vecspaceFactory);
	}
	else 
	{
	  //generate rules for the three levels
	  Sts.opts.extendDiffRulesLvl = Some(0)
	  val rulesLvl0 = new DiffRules().getRule(assumptions.head.expression, goal, ruleWeighter, vecspaceFactory);
	  Sts.opts.extendDiffRulesLvl = Some(1)
	  val rulesLvl1 = new DiffRules().getRule(assumptions.head.expression, goal, ruleWeighter, vecspaceFactory);
	  Sts.opts.extendDiffRulesLvl = Some(2)
	  val rulesLvl2 = new DiffRules().getRule(assumptions.head.expression, goal, ruleWeighter, vecspaceFactory);
	  
	  Sts.opts.extendDiffRulesLvl = None //return it to None again 

	  (rulesLvl0 ++ rulesLvl1 ++ rulesLvl2)
	}
    
	var newConstants: Map[String, Set[String]] = constants;
	def addConst(varName: String) =
		newConstants += (varName.substring(0, 2) -> (newConstants.apply(varName.substring(0, 2)) + varName))
		
    val rules:List[WeightedExpression[BoxerExpression]] = Sts.opts.inferenceRulesLevel match {
		case -1 => List();
		case _ => (precompiledRules ++  onthefulyRules ++ wordNetRules ++ diffRule ++ distributionalRules).flatMap(r=>{
			val result = Rules.createWeightedExpression(r._1, r._2, r._3, r._4, simplifiedDeclarations)
			result._2.map( c => addConst(c));
			result._1
		});
	 } 
  
    delegate.prove(newConstants, declarations, evidence, assumptions ++ (rules.toSet.toList), goal)
  }
}
