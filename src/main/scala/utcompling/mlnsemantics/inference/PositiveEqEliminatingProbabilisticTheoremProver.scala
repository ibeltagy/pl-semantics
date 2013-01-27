package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression

class PositiveEqEliminatingProbabilisticTheoremProver(
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

    val newAssumption = removeEq(assumptions.head.expression);
    val newGoal = removeEq(goal);
   
    val newAssumptions = assumptions.filterNot( _ == assumptions.head) ++ 
    					List (HardWeightedExpression (newAssumption));    

    delegate.prove(
      constants,
      declarations,
      evidence, 
      newAssumptions,
      newGoal)

  }
  
  private def removeEq(input: FolExpression): FolExpression =
  {
    val b = List[(Variable, Variable)]()
    	return input;
    /*input match {
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
   */
  }
  
  private def findEq(input: FolExpression): List[(String, String)] =
  {
    val b = List[(String, String)]()
    input match {
   	case FolExistsExpression(variable, term) => findEq(term) ;
   	case FolAllExpression(variable, term) => findEq(term);
   	case FolAndExpression(first, second) =>  findEq(first) ++  findEq(second);
   	case FolOrExpression(first, second) => findEq(first) ++  findEq(second);   	
   	case FolIfExpression(first, second) => findEq(first) ++  findEq(second);
   	case FolIffExpression(first, second) => findEq(first) ++  findEq(second);

   	//case FolNegatedExpression(term) => FolNegatedExpression(renameVars(term, namePrefix))
   	//case FolEqualityExpression(first, second) => FolEqualityExpression(renameVars(first, namePrefix), renameVars(second, namePrefix))

   	//case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(v => Variable(namePrefix+v.name)))
   	case FolVariableExpression(v) => ;
    case FolApplicationExpression(fun, arg) => findEq(fun);
   }
   return b;   
  }

}
