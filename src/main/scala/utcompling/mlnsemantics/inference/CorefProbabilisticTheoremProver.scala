package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import utcompling.mlnsemantics.run.Sts
import scala.collection.mutable.MutableList
import utcompling.mlnsemantics.inference.support.GoalExpression
import org.apache.commons.logging.LogFactory



class CorefProbabilisticTheoremProver(
	delegate: ProbabilisticTheoremProver[FolExpression])
	extends ProbabilisticTheoremProver[FolExpression] {

	private val LOG = LogFactory.getLog(classOf[CorefProbabilisticTheoremProver])
	private var coreferences:Map[String, (String, String)] = null;
	private var inferenceRules:Set[String] = null;
	private var quantifiersForRenameFun:List[String] = null;

	/**
	 * Return the proof, or None if the proof failed
	 */
	def prove(
		constants: Map[String, Set[String]], // type -> constant
		declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
		evidence: List[FolExpression],
		assumptions: List[WeightedExpression[FolExpression]],
		goal: FolExpression): Seq[Double] = {

      //all "quantified" variables have to be renamed to avoid conflict between variables and constants.
		//rename is moved from NoExist to here
		quantifiersForRenameFun = List();
		var newGoal = rename(goal);
		var newAssumptions = assumptions
		inferenceRules = Set();
		coreferences = Map();
		if (Sts.opts.coref )
		{
			//simple conjunction, one goal, no entailment predicate
			val goalPreds = findPreds(newGoal, false, false, List(), List())

			//----------------------
			val textPreds = assumptions.flatMap
			{
				case HardWeightedExpression(e, w) => {
					val preds = findPreds(e, false, false, List(), List()) 
					Some(preds)
				}
				case SoftWeightedExpression(e, w) => 
				{
					if(e.getVariables.size == 1 && w >= 0.9 ) //a rule of single entity with very high weight 
						inferenceRules = inferenceRules + e.toString;
					None
				}
				case _ => None;
			}.head
			
			//try to match positive existentially quantified predicates in T with negative universally quantified predicates in H
			textPreds._1.foreach (textPred => { 
				goalPreds._2.foreach (goalPred => {
					if (isCoreferring(textPred._1, goalPred._1)) //is coreferring
					{
						LOG.trace("Coref(Goal): " + textPred._2 + "->"  + goalPred._2)
						coreferences = coreferences + (goalPred._2 -> (goalPred._1, textPred._2))  //coreferring variables
					}
				})
			})
			newGoal = applyCoref(newGoal);
			LOG.trace("Goal: " + newGoal);
			//try to match negative universally quantified predicates in T with positive existentially quantified predicates in H			
			coreferences = coreferences.empty
			textPreds._2.foreach (textPred => { 
				goalPreds._1.foreach (goalPred => {
					//try to match positive existentially quantified predicates in T with negative universally quantified predicates in H
					if (isCoreferring(textPred._1, goalPred._1)) //same name, so coreferring
					{
						LOG.trace("Coref(Text): " + goalPred._2 + "->"  + textPred._2)
						coreferences = coreferences + (textPred._2 -> (textPred._1, textPred._2))  //coreferring variables
					}
				})
			})
			newAssumptions = assumptions.map
			{
				case HardWeightedExpression(e, w) => {
					var modifiedText = applyCoref(e) //remove quantifiers  
					coreferences.values.foreach(a => {
						val atom = FolAtom(Variable(a._1), Variable(a._2))
						modifiedText = FolExistsExpression(Variable(a._2), FolAndExpression(modifiedText, atom)) 
					}) // and replace them with EXIST
					LOG.trace("Text: " + modifiedText);
					HardWeightedExpression(modifiedText, w)
				}
				case _ @ a=> a;
			}
		}
		delegate.prove(constants, declarations, evidence, newAssumptions, newGoal)
	}
	private def isCoreferring(lhs:String, rhs:String) : Boolean = 
	{
		//Two entities are coreferring if they have the same name, or if there is an infernece rule connecting them.
		//"ASSUMING" that the rules are generated from Wordnet or DiffRule. 
		//Generating rules for "all pairs" is worng here. 
		//May be I need to filter the rules with their weights. 
		val filteredIR = inferenceRules.filter(ir => (lhs + ".*" + rhs + "|" + rhs + ".*" + lhs).r.findFirstIn(ir).size > 0 ) 
		return (lhs == rhs || (filteredIR.size > 0 && Sts.opts.corefOnIR)); 
	}
	private def findPreds(expr: FolExpression, inLhs:Boolean, isNegated:Boolean, univs:List[String], exists:List[String]): (List[(String, String)], List[(String, String)]) =
	{
		expr match {
		case FolExistsExpression(variable, term) => {
			if (isNegated) findPreds(term, inLhs, isNegated, univs :+ variable.name, exists)
			else findPreds(term, inLhs, isNegated, univs, exists :+ variable.name)
		}
		case FolAllExpression(variable, term) => {
			if (isNegated) findPreds(term, inLhs, isNegated, univs, exists :+ variable.name)
			else findPreds(term, inLhs, isNegated, univs :+ variable.name, exists)
		}
		case FolNegatedExpression(term) => findPreds(term, false, !isNegated, univs, exists);
		case FolOrExpression(first, second) => findPreds(FolAndExpression(first, second), inLhs, isNegated, univs, exists);
		case FolIfExpression(first, second) => 
		{
			val f = findPreds(first, true, !isNegated, univs, exists);
			val s = findPreds(second, false, isNegated, univs, exists);
			(f._1 ++ s._1, f._2 ++ s._2) 
		}
		case FolAndExpression(first, second) =>
		{
			val f = findPreds(first, inLhs, isNegated, univs, exists);
			val s = findPreds(second, inLhs, isNegated, univs, exists);
			(f._1 ++ s._1, f._2 ++ s._2)
		}
		case FolAtom(pred, args @ _*) =>
		{
			if (args.length == 1 && (pred.name.contains("_n_") || pred.name.contains("_v_") || pred.name.contains("group_head"))) //predicate is a noun or verb or group_head
			{
				assert(univs.contains(args.head.name) || exists.contains(args.head.name))
				val isUniv = univs.contains(args.head.name);
				if (!isNegated && !isUniv) //positive existentially quantified
					return (List((pred.name, args.head.name)), List())
				else if (isNegated && isUniv && !Sts.opts.evdIntroSingleVar && !inLhs) //negated universal that will be handled in SetGoal using fixCWA not fixUnivInQ
					return (List(), List((pred.name, args.head.name))) 
			}
			return (List(), List())
		}
		case FolVariableExpression(v) => (List(), List())
		case FolEqualityExpression(first, second) => (List(), List())
		case FolIffExpression(first, second) => throw new RuntimeException(expr + " is not a valid expression")      
		case _ => throw new RuntimeException(expr + " is not a valid expression")
		}
	}
	
	private def applyCoref(v: String): String =
		coreferences.getOrElse(v, ("", v))._2


	private def applyCoref(v: Variable): Variable =
		Variable(applyCoref(v.name))

	private def applyCoref(e: FolExpression): FolExpression = 
	{
		e match 
		{
			case FolExistsExpression(v, term) =>
				if (coreferences.contains(v.name))
					applyCoref(term) //remove the quantification on the variable
				else
					FolExistsExpression(v, applyCoref(term))	
			case FolAllExpression(v, term) => 
				if (coreferences.contains(v.name))
					applyCoref(term) //remove the quantification on the variable
				else
					FolAllExpression(v, applyCoref(term))
			case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(applyCoref(_)) :_ * )	
			case FolVariableExpression(v) => FolVariableExpression(applyCoref(v)) 
			case _=>e.visitStructured(applyCoref, e.construct)
		}
	}


  private def rename(v: Variable): Variable =
   if (quantifiersForRenameFun.contains(v.name))
      Variable(v.name + "_q")
   else v //this is a constant. Do not change the name

  private def rename(e: FolExpression): FolExpression =
  {
    e match {
      case FolExistsExpression(v, term) => quantifiersForRenameFun = quantifiersForRenameFun :+ v.name; FolExistsExpression(rename(v), rename(term))
      case FolAllExpression(v, term) => quantifiersForRenameFun = quantifiersForRenameFun :+ v.name; FolAllExpression(rename(v), rename(term))
      case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(rename(_)) :_ * )
      case FolVariableExpression(v) => FolVariableExpression(rename(v))
      case _=>e.visitStructured(rename, e.construct)
    }
  }


}
