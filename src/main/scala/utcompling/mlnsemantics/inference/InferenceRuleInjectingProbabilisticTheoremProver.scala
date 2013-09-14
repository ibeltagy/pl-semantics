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
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import org.apache.commons.logging.LogFactory
import support.HardWeightedExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerNamed
import utcompling.mlnsemantics.run.Sts
import dhg.depparse.Lemmatize

class InferenceRuleInjectingProbabilisticTheoremProver(
  wordnet: Wordnet,
  vecspaceFactory: ((String => Boolean) => Map[String, BowVector]),
  ruleWeighter: RuleWeighter,
  paraphraseRules: List[String],
  distWeight: Double = 1.0,
  resourceWeight: Double = 1.0,
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  private val LOG = LogFactory.getLog(classOf[InferenceRuleInjectingProbabilisticTheoremProver])

  private val NotPred = """^not_(.+)$""".r
  
  private var vectorspaceFormatWithPOS = false; 
  
  private var withPatientAgentInferenceRules = true;

  private var inferenceRulesLevel = 2;  //0: no IR, 1: words only, 2: words + phrases
  
  private var task = "sts";  //sts or rte 

  private def d(drs: BoxerExpression) =
    new Boxer2DrtExpressionInterpreter().interpret(
      new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(drs))

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {

    vectorspaceFormatWithPOS = Sts.opts.get("-vsWithPos") match {
		case Some(vst) => vst.toBoolean;
		case _ => false;
    }
 
    inferenceRulesLevel = Sts.opts.get("-irLvl") match {
		case Some(vst) => vst.toInt;
		case _ => 2;
    }
    
   
    withPatientAgentInferenceRules = Sts.opts.get("-peInf") match {
		case Some(vst) => vst.toBoolean;
		case _ => true;
    }

	task = Sts.opts.get("-task") match {
      case Some(t) => t;
      case _ => "sts";
    }

   
    assumptions.foreach(x => LOG.info("\n" + d(x.expression).pretty))
    val rules = inferenceRulesLevel match {
		case 0 => Set();
		case _ => makeNewRules(assumptions.map(_.expression), goal);
	 }
    LOG.info("\n" + d(goal).pretty)
    delegate.prove(constants, declarations, evidence, assumptions ++ rules, goal)
  }

  def makeNewRules(assumptions: List[BoxerExpression], goal: BoxerExpression): Set[WeightedExpression[BoxerExpression]] = {
    val assumPredsAndContexts = assumptions.flatMap(getAllPredsAndContexts)
    val goalPredsAndContexts = (List(goal)).flatMap(getAllPredsAndContexts)
    val allPredsAndContexts =  List.concat(assumPredsAndContexts, goalPredsAndContexts);
    val vectorspace = vecspaceFactory(allPredsAndContexts.flatMap {
      case (pred, context) => {
        val l = pred.name.split("_").map (n => n + (vectorspaceFormatWithPOS match {
        case true => "-" + pred.pos;
        case false => "";
        })) ++ context
        l
       }.map(stripNot)
    }.toSet)
    
    val assumRel = assumptions.flatMap(getAllRelations);
    val goalRel = List(goal).flatMap(getAllRelations);

	// Use this map to check variable types in inference rules
	var predTypeMap = Map[String, String]()
	for(assumPredsAndContext <- assumPredsAndContexts)
		predTypeMap += ((assumPredsAndContext._1.name ++ "_t") -> variableType(assumPredsAndContext._1))
	for(goalPredsAndContext <- goalPredsAndContexts)
		predTypeMap += ((goalPredsAndContext._1.name ++ "_h") -> variableType(goalPredsAndContext._1))

//    var rules = makeRules(assumPredsAndContexts, goalPredsAndContexts, vectorspace)
    //rules = rules ++ makeLongerRules(assumRel, goalRel, assumPredsAndContexts, goalPredsAndContexts, vectorspace);
    var rules = makeLongerRules(assumRel, goalRel, assumPredsAndContexts, goalPredsAndContexts, vectorspace, predTypeMap);
    //rules.foreach(x => LOG.info("\n" + d(x.expression).pretty))
    return rules
  }

  def getAllPredsAndContexts(e: BoxerExpression): Seq[(BoxerPred, Seq[String])] = {
    val preds = getAllPreds(e)
    val predsAndContexts = preds.zipWithIndex.map { case (p, i) => p -> (preds.take(i) ++ preds.drop(i + 1)) }
    val newPredsAndContexts = predsAndContexts.mapVals(_.map(p => (stripNot(p.name) + (vectorspaceFormatWithPOS match {
        case true => "-" +p.pos;
        case false => "";
    }))));
    val newPredsAndContextsSplit = newPredsAndContexts.mapVals(l=>{
      var flatL: List[String] = List();
      l.foreach(e=>{
        flatL = flatL ++ e.split("_");
      })
      flatL
    }) 
    
    return newPredsAndContextsSplit;//predsAndContexts.mapVals(_.map(p => stripNot(p.name))) ;
  }

  private def getAllPreds(e: BoxerExpression): Seq[BoxerPred] =
    e match {
      case p: BoxerPred => Seq(p)
      case BoxerNamed(discId, indices, variable, name, typ, sense) => Seq(BoxerPred(discId, indices, variable, name, "n", sense))
      case _ => e.visit(getAllPreds, (parts: List[Seq[BoxerPred]]) => parts.flatten, Seq.empty[BoxerPred])
    }
  
  private def getAllRelations(e: BoxerExpression): Seq[BoxerRel] =
    e match {
      case p: BoxerRel => {
    	if ((p.name == "patient" || p.name == "agent") && !withPatientAgentInferenceRules )  
    		Seq.empty[BoxerRel]
    	else Seq(p)
      }
      case _ => e.visit(getAllRelations, (parts: List[Seq[BoxerRel]]) => parts.flatten, Seq.empty[BoxerRel])
    }
 
  private def findRelPred(preds: Iterable[(BoxerPred, Iterable[String])], rel: Iterable[BoxerRel]): Set[(BoxerExpression, Iterable[String], String)] = {
    var mapPredVar  = (preds.map(row => row._1.variable.name -> row)).toMap;
    var notUsedPred = preds.toMap;

	  if (inferenceRulesLevel == 1) //no phrases
			mapPredVar = mapPredVar.empty
    
    rel.flatMap(r => {
    	if (mapPredVar.contains(r.event.name) && mapPredVar.contains(r.variable.name))
    	{
	    	val arg1 = mapPredVar(r.event.name)
	    	val arg2 = mapPredVar(r.variable.name)
	    	
	    	notUsedPred =  notUsedPred - arg1._1; 
	    	notUsedPred =  notUsedPred - arg2._1;

		val (varName1, varName2) = if(arg2._1.pos == "v") ("x1", "x0")
					else ("x0", "x1")
	    	
	    	val arg1Changed = BoxerPred(arg1._1.discId, arg1._1.indices, BoxerVariable(varName1), arg1._1.name, arg1._1.pos, arg1._1.sense)
	    	val arg2Changed = BoxerPred(arg2._1.discId, arg2._1.indices, BoxerVariable(varName2), arg2._1.name, arg2._1.pos, arg2._1.sense)
	    	val rChanged = BoxerRel(r.discId, r.indices, BoxerVariable(varName1), BoxerVariable(varName2), r.name, r.sense)
	    	
	    	//println ("//PHRASE(npn): " + arg1._1.name+"-"+arg1._1.pos + " " + r.name + " " + arg2._1.name+"-"+arg2._1.pos)
	    	val context = (arg1._2 ++ arg2._2).toList.diff(arg1._1.name.split("_")).diff(arg2._1.name.split("_"));
	    	var words = vectorspaceFormatWithPOS match {
	    		case true => arg1._1.name +"-" +arg1._1.pos + "_" + arg2._1.name+"-" +arg2._1.pos ;
	    		case false => arg1._1.name + "_" + arg2._1.name;
	    	}
	    		
	    	//val vars = List(List() ->BoxerVariable("x0")) ++ List(List() ->BoxerVariable("x1"))
	    	val vars = List();
	    	val cond = List(arg1Changed) ++ List(arg2Changed) ++ List(rChanged);
	    	val exp = BoxerDrs(vars, cond);
	    	Some((exp, context, words))
    	}
    	else None;
    }).toSet ++ (Sts.opts.get("-noDup") match {
                     case Some(s) => s.toBoolean match {
                        case false => preds;
                        case _ => notUsedPred;
                     }
                     case _ => notUsedPred;
                 }).map(p => (
        BoxerDrs(List(), List(BoxerPred(p._1.discId, p._1.indices, BoxerVariable("x1"), p._1.name, p._1.pos, p._1.sense))),
        p._2,
        vectorspaceFormatWithPOS match {case true => p._1.name +"-" + p._1.pos; case false => p._1.name;}  
        )) 
  
  }

	/**
	 * Convert paraphrase rules in text format to FOL.
	 */
	private def convertParaphraseToFOL(
		assumePreds: Iterable[BoxerPred],
		goalPreds: Iterable[BoxerPred],
		assumeRels: Iterable[BoxerRel], 
		goalRels: Iterable[BoxerRel]
	): List[(BoxerDrs, BoxerDrs, Double)] =
	{
		val assumePredsList = assumePreds.toList
		val goalPredsList = goalPreds.toList
		val assumeRelsList = assumeRels.toList
		val goalRelsList = goalRels.toList

		paraphraseRules.map { rule =>
			val Array(id, left, right, score) = rule.split("\t")

			val leftTokens = left.split(" ").flatMap(token => Array(token, Lemmatize(token, "")) )
						.distinct

			// Find relating predicates for the lhs
			val matchAssumePreds = assumePredsList.filter { pred => 
				var isContained = true
				pred.name.split("_").foreach { token =>
					if(token != "topic" && !leftTokens.contains(token) && !leftTokens.contains(token + "s")) 
						isContained = false
				}
				isContained
			}
			val matchAssumePredVars = matchAssumePreds.map(pred => pred.variable.name)
			val matchAssumeRels = assumeRelsList.filter(rel => 
				(matchAssumePredVars.contains(rel.event.name) && matchAssumePredVars.contains(rel.variable.name))
				|| ( (matchAssumePredVars.contains(rel.event.name) || matchAssumePredVars.contains(rel.variable.name)
						|| matchAssumePredVars.isEmpty
				     )
				     && leftTokens.contains(rel.name)
				   )
			)

			val rightTokens = right.split(" ").flatMap(token => Array(token, Lemmatize(token, "")) )
						.distinct

			// Find relating predicates for the rhs
			val matchGoalPreds = goalPredsList.filter { pred => 
				var isContained = true
				pred.name.split("_").foreach { token =>
					if(token != "topic" && !rightTokens.contains(token) && !rightTokens.contains(token + "s")) 
						isContained = false
				}
				isContained
			}
			val matchGoalPredVars = matchGoalPreds.map(pred => pred.variable.name)
			val matchGoalRels = goalRelsList.filter(rel => 
				(matchGoalPredVars.contains(rel.event.name) && matchGoalPredVars.contains(rel.variable.name))
				|| ( (matchGoalPredVars.contains(rel.event.name) || matchGoalPredVars.contains(rel.variable.name)
						|| matchGoalPredVars.isEmpty
				     )
				     && rightTokens.contains(rel.name)
				   )
			)

			// Use this map to rename variables in the rhs
			var varNameMap = Map[String, String]()

			val changedMatchGoalPreds = matchGoalPreds.map { pred =>
/*				if(matchAssumePreds.size == 1 && matchGoalPreds.size == 1) 
					varNameMap += (pred.variable.name -> matchAssumePreds(0).variable.name) 
				else matchAssumePreds.foreach { assumePred =>
					val assumePredName = ("_" + assumePred.name + "_").replaceAll("_topic_", "_")
					val goalPredName = ("_" + pred.name + "_").replaceAll("_topic_", "_")
					if( (assumePred.toString.contains(",v,") && pred.toString.contains(",v,")) ||
						assumePredName.contains(goalPredName) || 
						goalPredName.contains(assumePredName)
					)
						varNameMap += (pred.variable.name -> assumePred.variable.name) 
				}
*/
				BoxerPred(pred.discId, 
					pred.indices, 
					//BoxerVariable("rhs_" + pred.variable.name), 
					BoxerVariable(varNameMap.getOrElse(pred.variable.name, "rhs_" + pred.variable.name)), 
					pred.name, 
					pred.pos, 
					pred.sense)
			}
			val changedMatchGoalRels = matchGoalRels.map { rel =>
				BoxerRel(rel.discId, 
					rel.indices, 
					//BoxerVariable("rhs_" + rel.event.name), 
					//BoxerVariable("rhs_" + rel.variable.name), 
					BoxerVariable(varNameMap.getOrElse(rel.event.name, "rhs_" + rel.event.name)), 
					BoxerVariable(varNameMap.getOrElse(rel.variable.name, "rhs_" + rel.variable.name)), 
					rel.name, 
					rel.sense)
			}

			val leftFOL = matchAssumePreds ++ matchAssumeRels
			val rightFOL = changedMatchGoalPreds ++ changedMatchGoalRels
			(BoxerDrs(List(), leftFOL), BoxerDrs(List(), rightFOL), score.toDouble)
		}
	}

	/**
	 * Convert paraphrase rules in FOL format to MLF.
	 */
	private def paraphraseRuleFOL(leftFOL: BoxerDrs, rightFOL: BoxerDrs, score: Double): List[WeightedExpression[BoxerExpression]] =
	{
		val unweightedRule = BoxerImp("h", List(), leftFOL, rightFOL)
		return List(SoftWeightedExpression(unweightedRule, score * resourceWeight))
	}

	private def checkCompatibleType(
		assumeFOL: BoxerExpression, 
		goalFOL: BoxerExpression, 
		assumeVarTypeMap: Map[String, String], 
		goalVarTypeMap: Map[String, String]
	): Boolean =
	{
		// Check phrase type
		val BoxerDrs(_, assumeConds) = assumeFOL
		val BoxerDrs(_, goalConds) = goalFOL
		//if(assumeConds.size != goalConds.size) return false

		if(assumeFOL.toString.contains(",v,") && !goalFOL.toString.contains(",v,")) return false
		if(!assumeFOL.toString.contains(",v,") && goalFOL.toString.contains(",v,")) return false

		// Check if the NEs are different, return false.
		// The implementation is tricky and not totally correct.
		if(assumeConds.size == 3 && goalConds.size == 3)
		{
			var flag = 0

			val assumePrednamePos1 = assumeConds(0) match 
			{
				case BoxerPred(discId, indices, variable, name, pos, sense) => (name, pos)
			}

			val assumePrednamePos2 = assumeConds(1) match 
			{
				case BoxerPred(discId, indices, variable, name, pos, sense) => (name, pos)
			}

			val goalPrednamePos1 = goalConds(0) match 
			{
				case BoxerPred(discId, indices, variable, name, pos, sense) => (name, pos)
			}

			val goalPrednamePos2 = goalConds(1) match 
			{
				case BoxerPred(discId, indices, variable, name, pos, sense) => (name, pos)
			}
			
			if(assumePrednamePos1._2.length > 1 || goalPrednamePos1._2.length > 1)
			{
				if(assumePrednamePos1._1.contains(goalPrednamePos1._1)
					|| goalPrednamePos1._1.contains(assumePrednamePos1._1)
				)
					flag += 1
				else flag -= 1
			}

			if(assumePrednamePos1._2.length > 1 || goalPrednamePos2._2.length > 1)
			{
				if(assumePrednamePos1._1.contains(goalPrednamePos2._1)
					|| goalPrednamePos2._1.contains(assumePrednamePos1._1)
				)
					flag += 1
				else flag -= 1
			}

			if(assumePrednamePos2._2.length > 1 || goalPrednamePos1._2.length > 1)
			{
				if(assumePrednamePos2._1.contains(goalPrednamePos1._1)
					|| goalPrednamePos1._1.contains(assumePrednamePos2._1)
				)
					flag += 1
				else flag -= 1
			}

			if(assumePrednamePos2._2.length > 1 || goalPrednamePos2._2.length > 1)
			{
				if(assumePrednamePos2._1.contains(goalPrednamePos2._1)
					|| goalPrednamePos2._1.contains(assumePrednamePos2._1)
				)
					flag += 1
				else flag -= 1
			}
			
			if(flag < 0) return false
		}

		// Check argument type
		for(varName <- assumeVarTypeMap.keys)
			if(goalVarTypeMap.contains(varName) && (assumeVarTypeMap(varName) != goalVarTypeMap(varName))) return false

		return true
	}
  
  private def changeExpDirection (e: BoxerExpression): BoxerExpression =
  {
    e match {
      case BoxerRel(discId, indices, event, variable, name, sense) => BoxerRel(discId match {case "h"=>"t";case "t"=>"h";}, indices, event, variable, name, sense);
      case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discId match {case "h"=>"t";case "t"=>"h";}, indices, variable, name, pos, sense);
      case BoxerDrs(refs, conds) => BoxerDrs(List(List() -> BoxerVariable("x0")) ++ List(List() -> BoxerVariable("x1")), conds.map(changeExpDirection))
      case _ => e.visitConstruct(changeExpDirection);
    }
  }
  
  private def makeExpRule (assum: (BoxerExpression, Iterable[String], String), goal: (BoxerExpression, Iterable[String], String), discId: String, vectorspace: Map[String, BowVector], predTypeMap: Map[String, String], isEntail: Boolean) : List[WeightedExpression[BoxerExpression]] = {
   
    val rw = ruleWeighter.weightForRules(assum._3, assum._2, Seq((goal._3 , goal._2)).toMap, vectorspace);
    val changedAssum = changeExpDirection(assum._1);

	val assumeVarTypeMap = 
		if(isEntail) getAllPreds(assum._1).map(pred => (pred.variable.name, predTypeMap(pred.name ++ "_t"))).toMap
		else getAllPreds(assum._1).map(pred => (pred.variable.name, predTypeMap(pred.name ++ "_h"))).toMap
	val goalVarTypeMap = 
		if(isEntail) getAllPreds(goal._1).map(pred => (pred.variable.name, predTypeMap(pred.name ++ "_h"))).toMap
		else getAllPreds(goal._1).map(pred => (pred.variable.name, predTypeMap(pred.name ++ "_t"))).toMap
	
//    if (rw.head._2.get < 0.35)
    if (rw.head._2.get <=0 || !checkCompatibleType(changedAssum, goal._1, assumeVarTypeMap, goalVarTypeMap))
    	return List();
    
    //val unweightedRule = BoxerDrs(List(List() -> BoxerVariable("x0")) ++ List(List() -> BoxerVariable("x1")), List(BoxerImp(discId, List(), changedAssum, goal._1)));
    val unweightedRule = BoxerImp(discId, List(), changedAssum, goal._1);
    
    
    return List(SoftWeightedExpression(unweightedRule, rw.head._2.get * distWeight));
    /*
    val BoxerPred(aDiscId, aIndices, aVariable, aName, aPos, aSense) = antecedent
    val BoxerPred(cDiscId, cIndices, cVariable, cName, cPos, cSense) = consequent
    val v = BoxerVariable(variableType(antecedent))
    val unweightedRule =
      BoxerImp(aDiscId, aIndices,
        BoxerDrs(List(Nil -> v), List(BoxerPred(aDiscId, aIndices, v, aName, aPos, aSense))),
        BoxerDrs(Nil, List(BoxerPred(cDiscId, cIndices, v, cName, cPos, cSense))))
    weight match {
      case Some(w) => 
      case None => HardWeightedExpression(unweightedRule)
    }
  } 
  */
    
    //return List(); 
  }
  
  private def makeLongerRules(assumRel: Iterable[BoxerRel], goalRel:Iterable[BoxerRel], assumPredsAndContexts: Iterable[(BoxerPred, Iterable[String])], goalPredsAndContexts: Iterable[(BoxerPred, Iterable[String])], vectorspace: Map[String, BowVector], predTypeMap: Map[String, String]): Set[WeightedExpression[BoxerExpression]] = {
    val assumRelPred = findRelPred(assumPredsAndContexts, assumRel);
    val goalRelPred = findRelPred(goalPredsAndContexts, goalRel);
    
    var ret = List[WeightedExpression[BoxerExpression]] () ;
	for (goalEntry <- goalRelPred )
	{
		for (assumEntry <- assumRelPred )
		{
			//DO not add rules if the word is the same??? Why?
		     //Words should have the same POS and same entity type (individual or event)
			
		  //TODO: Think again about this. Are you sure you want to add rules between non-matching POS words 
		//FIXIT(done): The block below should be fixed to generate bidirectional rules in case of STS, and do not generate 
		//rules between similar words. Also, what is true and false in function makeExpRule?
		  if (//assumPred._1.pos == goalPred._1.pos && 
			    assumEntry._3 != goalEntry._3 )
			    //no need to check for the variable anymore before all of them are INDV now. 
			    //assumPred._1.variable.name.charAt(0) == goalPred._1.variable.name.charAt(0))
			{
		      ret = ret ++ makeExpRule(assumEntry, goalEntry, "h", vectorspace, predTypeMap, true);
				if(task == "sts")
			      ret = ret ++ makeExpRule(goalEntry, assumEntry, "t", vectorspace, predTypeMap, false);
			}
		//====================================
		}
	} 

	// Add paraphrase rules
	val paraphraseFOL = convertParaphraseToFOL(assumPredsAndContexts.map(_._1), goalPredsAndContexts.map(_._1), assumRel, goalRel)
	paraphraseFOL.foreach { ruleFOL =>
		val BoxerDrs(_, leftFOL) = ruleFOL._1
		val BoxerDrs(_, rightFOL) = ruleFOL._2
		if(!leftFOL.isEmpty && !rightFOL.isEmpty)
			ret = ret ++ paraphraseRuleFOL(ruleFOL._1, ruleFOL._2, ruleFOL._3)
	}

	return ret.toSet;
  }

  private def stripNot(p: String): String = {	
    p match { case NotPred(x) => x; case x => x } 
  }

  
  
  private def makeRules(assumPredsAndContexts: Iterable[(BoxerPred, Iterable[String])], goalPredsAndContexts: Iterable[(BoxerPred, Iterable[String])], vectorspace: Map[String, BowVector]): Set[WeightedExpression[BoxerExpression]] = {
    
    var ret = List[WeightedExpression[BoxerExpression]] () ;
	for (assumPred <- assumPredsAndContexts)
	{
		for (goalPred <- goalPredsAndContexts)
		{
			//DO not add rules if the word is the same
		     //Words should have the same POS and same entity type (individual or event)
			
		  //TODO: Think again about this. Are you sure you want to add rules between non-matching POS words 
		  if (//assumPred._1.pos == goalPred._1.pos && 
			    assumPred._1.name != goalPred._1.name &&
			    assumPred._1.variable.name.charAt(0) == goalPred._1.variable.name.charAt(0))
			{
				var pred = assumPred._1;
				var antecedentContext = assumPred._2;
				var rhs = Map(goalPred._1 -> goalPred._2);
				var goalPredsAndContextsByName = Map(goalPred._1.name -> rhs);
				//FIXIT(fixed): Coung changd the line below from "h" to "t". I believe it should remain "h". 
				//Test and fix
				pred = BoxerPred("h", pred.indices, pred.variable, pred.name, pred.pos, pred.sense);
				
				var compatible = true;
				for (checkGoal <- goalPredsAndContexts){
					if (checkGoal._1.name == pred.name &&
					    checkGoal._1.pos == pred.pos &&
					    checkGoal._1.variable.name.charAt(0) != pred.variable.name.charAt(0))
					  compatible = false;
				}
				if (compatible){
					val rule = makeRulesForPred(pred, antecedentContext, goalPredsAndContextsByName , vectorspace);
					ret = List.concat(ret, rule);
				}
				else {
				  println ("hh" + goalPred._1.toString() + assumPred._1.toString() + " \n");
				}

				if(task == "sts")
				{
						  var lhs = Map(assumPred._1 -> assumPred._2);
						  var assumPredsAndContextsByName = Map(assumPred._1.name -> lhs); 
						  pred = goalPred._1;
						  pred = BoxerPred("t", pred.indices, pred.variable, pred.name, pred.pos, pred.sense);
						  
						  compatible = true;
						  for (checkAssum <- assumPredsAndContexts){
							  if (checkAssum._1.name == pred.name &&
									checkAssum._1.pos == pred.pos &&
									checkAssum._1.variable.name.charAt(0) != pred.variable.name.charAt(0))
								 compatible = false;
						  }
						  if (compatible){
							  val rule = makeRulesForPred(pred, goalPred._2, assumPredsAndContextsByName , vectorspace);
							  ret = List.concat(ret, rule);
						  }
						  else {
							 println ("tt" + goalPred._1.toString() + assumPred._1.toString() + " \n");
						  }
				}				
			}
		}
	} 
	return ret.toSet;
  }


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
    //for ((consequent, weight) <- ruleWeighter.weightForRules(pred.name, antecedentContext, consequentAndContexts.map(r=>(r._1.name, r._2)).toMap, vectorspace))
    //	yield makeRule(pred, consequent, weight)
    val rws = ruleWeighter.weightForRules(pred.name, antecedentContext, consequentAndContexts.map(r=>(r._1.name, r._2)).toMap, vectorspace);
    List(makeRule(pred, consequentAndContexts.head._1, rws.head._2))
  }

  private def makeRulesForNegPred(pred: BoxerPred, antecedentContext: Iterable[String], predsAndContextsByName: Map[String, Map[BoxerPred, Iterable[String]]], vectorspace: Map[String, BowVector]) = {
    val NotPred(simplePredName) = pred.name
    //val synonymsAndHypernyms = getSynonyms(simplePredName, pred.pos) ++ getHyponyms(simplePredName, pred.pos) //skip checking wordnet
    val synonymsAndHypernyms = predsAndContextsByName.keySet
    val consequentAndContexts = synonymsAndHypernyms.flatMap(predsAndContextsByName.get).flatten.filter(_._1 != pred)
      .filter(_._1.name match { case NotPred(_) => true; case _ => false })
    //for ((consequent, weight) <- ruleWeighter.weightForRules(pred.name, antecedentContext, consequentAndContexts.map(r=>(r._1.name, r._2)).toMap, vectorspace))
    //  yield makeRule(pred, consequent, weight)
    val rws = ruleWeighter.weightForRules(pred.name, antecedentContext, consequentAndContexts.map(r=>(r._1.name, r._2)).toMap, vectorspace);
    List(makeRule(pred, consequentAndContexts.head._1, rws.head._2))
  }
  
  private def makeRule(antecedent: BoxerPred, consequent: BoxerPred, weight: Option[Double]): WeightedExpression[BoxerExpression] = {
    val BoxerPred(aDiscId, aIndices, aVariable, aName, aPos, aSense) = antecedent
    val BoxerPred(cDiscId, cIndices, cVariable, cName, cPos, cSense) = consequent
    val v = BoxerVariable(variableType(antecedent))
    val unweightedRule =
      BoxerImp(aDiscId, aIndices,
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
