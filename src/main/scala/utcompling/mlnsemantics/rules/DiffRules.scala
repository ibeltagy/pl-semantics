package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.run.Sts
import scala.Array.canBuildFrom
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.mlnsemantics.datagen.Lemmatize
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.util.Resolution
import utcompling.mlnsemantics.util.InferenceRule
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs

class DiffRules {
  private val LOG = LogFactory.getLog(classOf[DiffRules])

  // Search phrases in Text-Hypothesis pair

  def getRule(text: BoxerExpression, hypothesis: BoxerExpression) : List[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] =
  {
  	if (!Sts.opts.diffRules)
  		return List();
	//List(List("deer(d1)"), List("agent(j1, d1)"), List("jump(j1)"), List("over(j1, f1)"), List("wall(f1)")), 
	//List(List("-deer(d1)", "-agent(j1, d1)", "-jump(j1)", "-over(j1)", "-patient(j1, f1)", "-fence(f1)") ))
  	val textPreds = text.getPredicates
  	val textRels = text.getRelations 
  	val hypPreds = hypothesis.getPredicates
  	val hypRels = hypothesis.getRelations  	
  	val textAtoms = (textPreds ++ textRels).map(boxerExpsToString).toMap
  	val hypAtoms = (hypPreds ++ hypRels).map(boxerExpsToString).toMap
  	val textAtomsRR = textAtoms.values.map(t => List((t._2))).toList
  	val hypAtomsRR = List(hypAtoms.values.map(t =>  "-" + t._2).toList)
  	val result = Resolution.resolveToFindDifference(textAtomsRR, hypAtomsRR);
  	println("DiffRules(TXT): " + textAtoms);
  	println("DiffRules(HYP): " + hypAtoms);
  	if (result.isDefined)
  	{
  		val ir:InferenceRule = result.get._1;
  		println ("DiffRules(DIF): " + ir)
  		if (Sts.goldStandard == 1)
	  	{
  			val lhsExps:Set[BoxerExpression] = ir.lhs.map(t => stringToBoxerExp(t, textAtoms)).toSet
  			val rhsExps:Set[BoxerExpression] = ir.rhs.map(t => stringToBoxerExp(t, hypAtoms)).toSet
  			println(lhsExps)
  			println(rhsExps)
  			val lhsDrs = Rules.boxerAtomsToBoxerDrs(lhsExps);
  			val rhsDrs = Rules.boxerAtomsToBoxerDrs(rhsExps);
  			return List((lhsDrs, rhsDrs, Double.PositiveInfinity, RuleType.Implication))
	  	}
  	}
  	return List()

  }
  def stringToBoxerExp(s:String, termsMap: Map[String,(BoxerExpression, String)]): BoxerExpression = 
  {
  	val splits = s.split("\\(")
  	val varsNames = splits(1).substring(0, splits(1).length() - 1).split(",").map(_.toLowerCase());
  	val entry = termsMap.get(splits(0));
  	assert (entry.isDefined);
  	entry.get._1 match 
  	{
  		case BoxerPred(discId, indices, variable, name, pos, sense) => assert(varsNames.length == 1);  return BoxerPred(discId, indices, BoxerVariable(varsNames(0)), name, pos, sense)
		case BoxerRel(discId, indices, event, variable, name, sense) => assert(varsNames.length == 2); return BoxerRel(discId, indices, BoxerVariable(varsNames(0)), BoxerVariable(varsNames(1)), name, sense)
		case _ => throw new RuntimeException("Unexpected BoxerExpression: " + s);
	}
  	throw new RuntimeException("Unreachable");
  }
  def boxerExpsToString(s:BoxerExpression) : (String, (BoxerExpression, String)) = 
  {
  	s match 
  	{
  		case BoxerPred(discId, indices, variable, name, pos, sense) => (name + "-" + pos, (s, name + "-" + pos + "(" + variable.name + ")"))
		case BoxerRel(discId, indices, event, variable, name, sense) => (name + "-r" , (s, name + "-r(" + event.name + "," + variable.name +")" ));
		case _ => throw new RuntimeException("Unexpected BoxerExpression: " + s);
	}
  }
}

