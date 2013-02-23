package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression

class HardAssumptionAsEvidenceProbabilisticTheoremProver(
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

    def go(e: FolExpression): List[FolExpression] = {
      e match {
        case FolAndExpression(first, second) => go(first) ++ go(second)
        case FolNegatedExpression(term) => List(); //because of the prior, everything is already negated 
        case FolEqualityExpression(first, second) => List();  //do not add equality constrains in any evidences. It is already handeled by variable renaming
        case FolAllExpression(v, term) => {
          val newV = v.name.toUpperCase

          term match {
            case FolIfExpression (first, second) => go(first.replace(v, FolVariableExpression(Variable(newV)))) ++ List(renameVars(e))
            case _ => List(renameVars(e));
          }
          //remove the outer forAll in a->b^c^... so that all variables are converted to constants
          //rename the variable to uppercase
          //go(term.replace(v, FolVariableExpression(Variable(newV))))
        }
        case _ => List(renameVars(e));
      }
    }

    val (evidenceAssumptions: List[WeightedExpression[FolExpression]], newAssumptions: List[WeightedExpression[FolExpression]]) =
      assumptions
        .flatMap {
          case HardWeightedExpression(e) => {
        	  go(e).map(HardWeightedExpression(_))
          }
          case a @ SoftWeightedExpression(e, w) => List(a)
        }
        .partition {
          case HardWeightedExpression(e @ FolAtom(_, _*)) => true
          case _ => false
        }

    delegate.prove(
      constants,
      declarations,
      evidence ++ evidenceAssumptions.map { case HardWeightedExpression(e) => e },
      newAssumptions,
      goal)

  }
  
  private def renameVars(input: FolExpression): FolExpression = {
    input match {
   	case FolExistsExpression(variable, term) => FolExistsExpression (variable, renameVars(term)) ;
   	case FolAllExpression(variable, term) => FolAllExpression (variable, renameVars(term)) ;
   	case FolNegatedExpression(term) => FolNegatedExpression(renameVars(term))
   	case FolAndExpression(first, second) => FolAndExpression(renameVars(first), renameVars(second))
   	case FolOrExpression(first, second) => FolOrExpression(renameVars(first), renameVars(second))   	
   	case FolIfExpression(first, second) => FolIfExpression(renameVars(first), renameVars(second))
   	case FolIffExpression(first, second) => FolIffExpression(renameVars(first), renameVars(second))
   	case FolEqualityExpression(first, second) => FolEqualityExpression(renameVars(first), renameVars(second))
   	//case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(v => Variable(namePrefix+v.name)))
   	case FolVariableExpression(v) => FolVariableExpression(v)
    case FolApplicationExpression(fun, arg) =>{
        fun match {
          case FolVariableExpression (v) => {
            var newName = v.name;
            //if (newName.startsWith("r_")) //if it is a relation predicate
        	//{
            newName = newName.replace("_dh", "_XXX");
            newName = newName.replace("_dt", "_YYY");
            newName = newName.replace("_XXX", "_dt");
            newName = newName.replace("_YYY", "_dh");
        	//}
        	FolApplicationExpression(FolVariableExpression(Variable(newName)), arg) 
          }
          case _=> FolApplicationExpression (renameVars(fun), renameVars(arg))
        }
    }
    }        
  }
}
