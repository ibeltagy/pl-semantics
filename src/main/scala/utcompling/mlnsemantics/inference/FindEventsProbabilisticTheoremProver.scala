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
	var propVars = findPropVar(newAssumption);
    newAssumption = convertToEvntPropVar(newAssumption);
    
    eventVars = findEventVar(newGoal);
	propVars = findPropVar(newGoal);
    newGoal = convertToEvntPropVar(newGoal);
    
    def convertToEvntPropVar(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerRel(discId, indices, event, variable, name, sense) =>
          		BoxerRel(discId, indices, convertVarToEvntPropVar(event), convertVarToEvntPropVar(variable), name, sense);
        case BoxerPred(discId, indices, variable, name, pos, sense) => 
          			BoxerPred(discId, indices, convertVarToEvntPropVar(variable), name, pos, sense)
        case BoxerAlfa(variable, first, second) => BoxerAlfa(convertVarToEvntPropVar(variable), first, second);
        case BoxerDrs (ref, cond) => BoxerDrs (ref.map ( (listRef:(List[BoxerIndex], BoxerVariable)) =>{
          (listRef._1, convertVarToEvntPropVar(listRef._2))
        } ), cond.map (convertToEvntPropVar));
        case BoxerCard(discId, indices, variable, num,typ) =>
          			BoxerCard(discId, indices, convertVarToEvntPropVar(variable), num,typ)
        case BoxerProp(discId, indices, variable, drs) => 
          				BoxerProp(discId, indices, convertVarToEvntPropVar(variable), convertToEvntPropVar(drs))
        case BoxerEq(discId, indices, first, second) => BoxerEq(discId, indices, convertVarToEvntPropVar(first), convertVarToEvntPropVar(second))
        case BoxerNamed(discId, indices, variable, name, typ, sense) => BoxerNamed(discId, indices, convertVarToEvntPropVar(variable), name, typ, sense)
        case BoxerTimex(discId, indices, variable, timeExp) => BoxerTimex(discId, indices, convertVarToEvntPropVar(variable), timeExp);
        case _ => e.visitConstruct(convertToEvntPropVar)
      }
    }
    
    def convertVarToEvntPropVar(v: BoxerVariable): BoxerVariable = {
    	if (eventVars.contains(v))
    		return BoxerVariable("e" + v.name.substring(1))
	else if(propVars.contains(v))
		return BoxerVariable("p" + v.name.substring(1))
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

	private def findPropVar(e: BoxerExpression): List[BoxerVariable] = {
      		e match {
        		case BoxerProp(discId, indices, variable, drs) => return List(variable) ++ findPropVar(drs)
        		case _ => {
          			e.visit(findPropVar, (parts: List[List[BoxerVariable]]) => {
            				var compined = List[BoxerVariable]();
            				for(p <- parts)
              					compined  = compined ++ p;
            				return compined.toSet.toList
          			}  ,List())
        		}
      		}
   	}
  
  
}
