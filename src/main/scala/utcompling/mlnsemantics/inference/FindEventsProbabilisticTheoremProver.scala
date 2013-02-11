package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import scala.collection.mutable.SetBuilder
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.mlnsemantics.wordnet.Wordnet
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import org.apache.commons.logging.LogFactory
import support.HardWeightedExpression

class FindEventsProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {  
  
  private val LOG = LogFactory.getLog(classOf[FindEventsProbabilisticTheoremProver])

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {
    
    var newAssumption =  assumptions.head.expression;
    var newGoal = goal; 
    
    var eventVars = findEventVar(newAssumption);
    newAssumption = convertToEvntVar(newAssumption);
    
    eventVars = findEventVar(newGoal);
    newGoal = convertToEvntVar(newGoal);
    
    def convertToEvntVar(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerRel(discId, indices, event, variable, name, sense) =>
          		BoxerRel(discId, indices, convertVarToEvntVar(event), variable, name, sense);
        case BoxerPred(discId, indices, variable, name, pos, sense) => 
          			BoxerPred(discId, indices, convertVarToEvntVar(variable), name, pos, sense)
        case BoxerDrs (ref, cond) => BoxerDrs (ref.map ( (listRef:(List[BoxerIndex], BoxerVariable)) =>{
          (listRef._1, convertVarToEvntVar(listRef._2))
        } ), cond.map (convertToEvntVar));
        case BoxerCard(discId, indices, variable, num,typ) =>
          			BoxerCard(discId, indices, convertVarToEvntVar(variable), num,typ)
        case BoxerProp(discId, indices, variable, drs) => 
          				BoxerProp(discId, indices, convertVarToEvntVar(variable), convertToEvntVar(drs))
        case BoxerEq(discId, indices, first, second) => BoxerEq(discId, indices, convertVarToEvntVar(first), convertVarToEvntVar(second))
        case BoxerNamed(discId, indices, variable, name, typ, sense) => BoxerNamed(discId, indices, convertVarToEvntVar(variable), name, typ, sense)
        case BoxerTimex(discId, indices, variable, timeExp) => BoxerTimex(discId, indices, convertVarToEvntVar(variable), timeExp);
        case _ => e.visitConstruct(convertToEvntVar)
      }
    }
    
    def convertVarToEvntVar(v: BoxerVariable): BoxerVariable = {
    	if (eventVars.contains(v))
    		return BoxerVariable("e" + v.name.substring(1))
    	else 
    		return v;
    }
    val newAssumptions = List(HardWeightedExpression(newAssumption))
    delegate.prove(constants, declarations, evidence, newAssumptions, newGoal)
  }
  
  
  private def findEventVar(e: BoxerExpression): List[BoxerVariable] = {
      e match {
        case BoxerRel(discId, indices, event, variable, name, sense) => {
          if (name == "agent" || name == "patient")
        	  return List(event);
          else 
            return List();
        }
        case _ => {
          e.visit(findEventVar, (parts: List[List[BoxerVariable]]) => {
            var compined = List[BoxerVariable]();
            for(p <- parts)
              compined  = compined ++ p;
            return compined.toSet.toList
          }  ,List())
        }
      }
   }
  
  
}