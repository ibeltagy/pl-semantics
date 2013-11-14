package utcompling.mlnsemantics.inference

import scala.collection.JavaConversions._
import scala.collection.mutable.SetBuilder
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.BowVector
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
  
  
  private def eliminateRedundantEquality(e: BoxerExpression): Option[BoxerExpression] = {
    e match {
      case BoxerDrs(refs, conds) =>{ 
        val drs = BoxerDrs(refs, conds.flatMap( c => {
          c match {
            case BoxerEq(discId, indices, first, second) => if (first.name != second.name) List(c) else None;  
            case _ => eliminateRedundantEquality(c)
          }
       }));
       if (drs.conds.length == 0) return None;
       return Some(drs)
      };
      
      case BoxerAlfa(variable, first, second) =>
        val firstMod = eliminateRedundantEquality(first);
        val secondMod = eliminateRedundantEquality(second);
        if (firstMod  == None && secondMod  == None)
          return None;
        else if (firstMod  == None)
          return secondMod;
        else if (secondMod  == None)
          return firstMod;
        else Some(BoxerAlfa(variable, firstMod.get, secondMod.get));
        
      case BoxerImp(discId, indices, first, second) =>
        val firstMod = eliminateRedundantEquality(first);
        val secondMod = eliminateRedundantEquality(second);
        if (firstMod  == None && secondMod  == None)
          return None;
        else if (firstMod  == None)
          return secondMod;
        else if (secondMod  == None)
          return firstMod;
        else Some(BoxerImp(discId, indices, firstMod.get, secondMod.get));

      case BoxerEqv(discId, indices, first, second) =>
        val firstMod = eliminateRedundantEquality(first);
        val secondMod = eliminateRedundantEquality(second);
        if (firstMod  == None && secondMod  == None)
          return None;
        else if (firstMod  == None)
          return secondMod;
        else if (secondMod  == None)
          return firstMod;
        else Some(BoxerEqv(discId, indices, firstMod.get, secondMod.get));
        
      case BoxerOr(discId, indices, first, second) =>
        val firstMod = eliminateRedundantEquality(first);
        val secondMod = eliminateRedundantEquality(second);
        if (firstMod  == None && secondMod  == None)
          return None;
        else if (firstMod  == None)
          return secondMod;
        else if (secondMod  == None)
          return firstMod;
        else Some(BoxerOr(discId, indices, firstMod.get, secondMod.get));
        
      case BoxerMerge(pred, first, second) =>
        val firstMod = eliminateRedundantEquality(first);
        val secondMod = eliminateRedundantEquality(second);
        if (firstMod  == None && secondMod  == None)
          return None;
        else if (firstMod  == None)
          return secondMod;
        else if (secondMod  == None)
          return firstMod;
        else Some(BoxerMerge(pred, firstMod.get, secondMod.get));        

      case BoxerNot(discId, indices, drs) =>
        val drsMod = eliminateRedundantEquality(drs);
        if (drsMod  == None)
          return None;
        else Some(BoxerNot(discId, indices, drsMod.get));        

      case BoxerProp(discId, indices, variable, drs) =>
        val drsMod = eliminateRedundantEquality(drs);
        if (drsMod  == None)
          return None;
        else Some(BoxerProp(discId, indices, variable, drsMod.get));
        
     //case BoxerApp(function, argument) => Some(BoxerApp(function, argument))
        
      case _ => Some(e)//e.visitConstruct(eliminateRedundantEquality)
    }
  }  
  private def mergePred(e: BoxerExpression): BoxerExpression = {
    
    val pred =  getAllPreds(e);
    var varPredMap: Map[String, List[BoxerPred]]= Map();
    
    pred.foreach(p => {
      val predList = varPredMap.get(p.variable.name) match {
        case Some(l) => l
        case None => List();
      };
      varPredMap = varPredMap + (p.variable.name -> (p::predList))
    })
    //Print data for later processing
    /*println("MaxPredicatesCount: "  + varPredMap.map(_._2.size).max)
    varPredMap.foreach(p =>
      if (p._2.length>1)
      {
        val names = p._2.map(e=> e.name + "-" + e.pos);
    	println("//PHRASE(nn): " + names.mkString(" ") )
      }
      else
        println("//PHRASE(n): " + p._2.head.name+ "-"+p._2.head.pos))
		*/

    def combinePred(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerPred(discId, indices, variable, name, pos, sense) =>{
          varPredMap.get(variable.name) match {
	          case Some(l) => {
	            varPredMap = varPredMap - variable.name;
	            BoxerPred(discId, indices, variable, l.map(_.name).mkString("_"), pos, sense);
	          }
	          case None => BoxerEq(discId, indices, variable, variable)
          };
        }          			
        case BoxerNamed(discId, indices, variable, name, typ, sense) => 
        {
          varPredMap.get(variable.name) match {
	          case Some(l) => {
	            varPredMap = varPredMap - variable.name;
	            BoxerPred(discId, indices, variable, l.map(_.name).mkString("_"), "n", sense);
	          }
	          case None => BoxerEq(discId, indices, variable, variable)
          };
        }
        case _ => e.visitConstruct(combinePred)
      }
    }
    return eliminateRedundantEquality(combinePred(e)).get;
  }
}
