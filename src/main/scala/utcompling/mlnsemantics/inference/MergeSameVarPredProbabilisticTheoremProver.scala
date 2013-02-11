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

class MergeSameVarPredProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {  
  
  private val LOG = LogFactory.getLog(classOf[MergeSameVarPredProbabilisticTheoremProver])

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {


    val newGoal = mergePred(goal);
       
    val newAssumptions: List[WeightedExpression[BoxerExpression]] =
      assumptions.map {
        case HardWeightedExpression(e) => HardWeightedExpression(mergePred(e))
        case SoftWeightedExpression(e, w) => SoftWeightedExpression(mergePred(e), w)
      }
    delegate.prove(constants, declarations, evidence, newAssumptions, newGoal)
  }
  
  private def getAllPreds(e: BoxerExpression): Seq[BoxerPred] = {
    e match {
      case p: BoxerPred => Seq(p)
      case BoxerNamed(discId, indices, variable, name, typ, sense) => Seq(BoxerPred(discId, indices, variable, name, "n", sense))
      case _ => e.visit(getAllPreds, (parts: List[Seq[BoxerPred]]) => parts.flatten, Seq.empty[BoxerPred])
    }
  }
  
  private def mergePred(e: BoxerExpression): BoxerExpression = {
    
    val pred =  getAllPreds(e);
    var varPredMap: Map[String, List[String]]= Map();
    
    pred.foreach(p => {
      val predList = varPredMap.get(p.variable.name) match {
        case Some(l) => l
        case None => List();
      };
      varPredMap = varPredMap + (p.variable.name -> (p.name::predList))
    })
    println("MaxPredicatesCount: "  + varPredMap.map(_._2.size).max)
    def combinePred(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerPred(discId, indices, variable, name, pos, sense) =>{
          varPredMap.get(variable.name) match {
	          case Some(l) => {
	            varPredMap = varPredMap - variable.name;
	            BoxerPred(discId, indices, variable, l.mkString("_"), pos, sense);
	          }
	          case None => BoxerEq(discId, indices, variable, variable)
          };
        }          			
        case BoxerNamed(discId, indices, variable, name, typ, sense) => 
        {
          varPredMap.get(variable.name) match {
	          case Some(l) => {
	            varPredMap = varPredMap - variable.name;
	            BoxerPred(discId, indices, variable, l.mkString("_"), "n", sense);
	          }
	          case None => BoxerEq(discId, indices, variable, variable)
          };
        }
        case _ => e.visitConstruct(combinePred)
      }
    }
    return combinePred(e);
  }
}
