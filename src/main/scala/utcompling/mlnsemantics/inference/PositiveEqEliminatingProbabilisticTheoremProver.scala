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
   * Assuming: 
   * --negated equality expressions are like !eq(x1,x2) and it is never 
   * !( eq(x1,x2) ^ ..... ^ ... )
   * --equality expressions are between variables, not formulas
   * --no duplicate equality expressions
   * --equality expressions like x1 = x2 ^ x2 = x3 will cause a problem, but 
   * we do not care. Let's hope this case does not happen
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Option[Double] = {

    val equalities = findEq(goal);
    val newAssumption = removeEq(assumptions.head.expression, equalities);
    
    val newGoal = removeEq(goal, equalities);
   
    val newAssumptions = List (HardWeightedExpression (newAssumption)) ++ 
    					assumptions.filterNot( _ == assumptions.head);
    
    val newDeclarations = for (declaration <- declarations)
    					yield {
      
    }
    delegate.prove(
      constants,
      declarations,
      evidence, 
      newAssumptions,
      newGoal)
  }
  
  private def removeEq(input: FolExpression, eq: List[(String, String)]): FolExpression =
  {
    input match {
   	case FolExistsExpression(variable, term) => FolExistsExpression (removeEq(variable, eq), removeEq(term, eq));

   	//This is very important: Changing all IMP to AND because alchamy is very slow on processing IMP
   	//case FolAllExpression(variable, term) => FolAllExpression(removeEq(variable, eq), removeEq(term, eq));
   	//case FolIfExpression(first, second) => FolIfExpression(removeEq(first, eq), removeEq(second, eq))
   	case FolAllExpression(variable, term) => removeEq(term, eq);
   	case FolIfExpression(first, second) => FolAndExpression(removeEq(first, eq), removeEq(second, eq))
   	
   	case FolNegatedExpression(term) => FolNegatedExpression(removeEq(term, eq))
   	case FolAndExpression(first, second) => FolAndExpression(removeEq(first, eq), removeEq(second, eq))
   	case FolOrExpression(first, second) => FolOrExpression(removeEq(first, eq), removeEq(second, eq))   	
   	case FolIffExpression(first, second) => FolIffExpression(removeEq(first, eq), removeEq(second, eq))
   	case FolEqualityExpression(first, second) => FolEqualityExpression(first, second)//do not apply it to eq expr
   	//case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(v => Variable(namePrefix+v.name)))
   	case FolVariableExpression(v) => FolVariableExpression(removeEq(v, eq))
    case FolApplicationExpression(fun, arg) => FolApplicationExpression(removeEq(fun, eq), removeEq(arg, eq))        
   }
  }
  
  private def removeEq(input: Variable, eq: List[(String, String)]): Variable=
  {
    for (val p <- eq)
    {
    	if (p._2.equals(input.name))
    	  return Variable(p._1);
    }
    return input;
  }
  
  private def findEq(input: FolExpression): List[(String, String)] =
  {
    //val b = List[(String, String)]()
    input match {
   	case FolExistsExpression(variable, term) => findEq(term) ;
   	case FolAllExpression(variable, term) => findEq(term);
   	case FolAndExpression(first, second) =>  findEq(first) ++  findEq(second);
   	case FolOrExpression(first, second) => findEq(first) ++  findEq(second);   	
   	case FolIfExpression(first, second) => findEq(first) ++  findEq(second);
   	case FolIffExpression(first, second) => findEq(first) ++  findEq(second);

   	case FolNegatedExpression(term) => {
   	 term match {
   	   case FolEqualityExpression(first, second) => List();
   	   case _ => findEq(term);
   	 } 
   	}
   	
   	case FolEqualityExpression(first, second) => {
   	  List((first match {
   	    case FolVariableExpression(variable) => variable.name
   	    case _ => throw new Exception ("Unrecongnized format, argumets of equality expression should be variables");
   	  },
   	  second match {
   	    case FolVariableExpression(variable) => variable.name
   	    case _ => throw new Exception ("Unrecongnized format, argumets of equality expression should be variables");
   	  }))
   	}

   	//case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(v => Variable(namePrefix+v.name)))
   	case FolVariableExpression(v) => List();
    case FolApplicationExpression(fun, arg) => findEq(fun);
   }
   //return b;   
  }

}
