package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import support.HardWeightedExpression

class FromEntToEqvProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Option[Double] = {

    
    val prem = renameVars(assumptions.head.expression, "t");
    val hyp = renameVars(goal, "h");
    val newGoal =  andingGoals (prem, hyp);
    
    
    //add premise and hypothesis to assumptions. 
    val newAssumptions = assumptions.filterNot( _ == assumptions.head) ++ 
    					List (HardWeightedExpression (hyp)) ++ 
    					List (HardWeightedExpression (prem)) ; 

    delegate.prove(
      constants,
      declarations,
      evidence, 
      newAssumptions,
      newGoal)

  }
  
  private def renameVars(input: FolExpression, namePrefix: String): FolExpression =
  {
    input match {
    case FolParseExpression(exps) => FolParseExpression( exps.map (e=> ( renameVars(e._1, namePrefix), e._2)) ) ;
   	case FolExistsExpression(variable, term) => FolExistsExpression (Variable(namePrefix+variable.name), renameVars(term, namePrefix)) ;
   	case FolAllExpression(variable, term) => FolAllExpression (Variable(namePrefix+variable.name), renameVars(term, namePrefix)) ;
   	case FolNegatedExpression(term) => FolNegatedExpression(renameVars(term, namePrefix))
   	case FolAndExpression(first, second) => FolAndExpression(renameVars(first, namePrefix), renameVars(second, namePrefix))
   	case FolOrExpression(first, second) => FolOrExpression(renameVars(first, namePrefix), renameVars(second, namePrefix))   	
   	case FolIfExpression(first, second) => FolIfExpression(renameVars(first, namePrefix), renameVars(second, namePrefix))
   	case FolIffExpression(first, second) => FolIffExpression(renameVars(first, namePrefix), renameVars(second, namePrefix))
   	case FolEqualityExpression(first, second) => FolEqualityExpression(renameVars(first, namePrefix), renameVars(second, namePrefix))
   	//case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(v => Variable(namePrefix+v.name)))
   	case FolVariableExpression(v) => FolVariableExpression(Variable(namePrefix+v.name))
    case FolApplicationExpression(fun, arg) =>{
        fun match {
          case FolVariableExpression (v) => FolApplicationExpression (fun, renameVars(arg, namePrefix))
          case _=> FolApplicationExpression (renameVars(fun, namePrefix), renameVars(arg, namePrefix))
        }
    }        
   }
  }
  
  private def andingGoals(g1: FolExpression, g2: FolExpression): FolExpression =
  {
    g1 match {
   	case FolExistsExpression(variable, term) => FolExistsExpression (variable, andingGoals(term, g2)) ;
   	case _ => g2 match {
   		case FolExistsExpression(variable, term) => FolExistsExpression (variable, andingGoals(g1, term)) ;
   		case _ => FolAndExpression (g1, g2);
   	}
   }
  }
  
}
