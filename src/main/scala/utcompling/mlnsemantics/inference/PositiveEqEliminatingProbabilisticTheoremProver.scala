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

    val goalEqualities = findEq(goal);
    val newGoal = removeEq(goal, goalEqualities);
    
    val assumEqualities = findEq(assumptions.head.expression);
    val newAssumption = removeEq(assumptions.head.expression, assumEqualities);
   
    val newAssumptions = List (HardWeightedExpression (newAssumption)) ++ 
    					assumptions.filterNot( _ == assumptions.head);
    
    /*val newDeclarations = for (declaration <- declarations)
    					yield {
    	var declarationTypes = declaration._2;
    	var declarationFol = declaration._1 match {
    	  case FolAtom(pred, args @ _*) => {
     		 
     		 val equalities = pred.name.charAt(pred.name.length()-1) match {
     		   case 'h' => goalEqualities;
     		   case 't' => assumEqualities;
     		 }
     		 val argsCount = args.length;
     		 val arg = declaration._1.asInstanceOf[FolApplicationExpression].argument;
     		 val fun = declaration._1.asInstanceOf[FolApplicationExpression].function;
    	     var newArg = arg; 
	         for (p <- equalities)
		     {
		    	val origVarName = arg.asInstanceOf[FolVariableExpression].variable.name;
    	    	if (p._2.equals(origVarName))
		    	{
		    	  newArg = FolVariableExpression(Variable(p._1));
	    	     //set type
		    	  val typ = p._1.charAt(0) match {
			      	case 'x' => "indv"
			        case 'e' => "evnt"
			        case 'p' => "prop"
			        case _ => "indv"
		    	  }
		    	  declarationTypes = declarationTypes.updated(argsCount-1, typ)
		    	}
		     }
	         
	    	 var newFun = fun match {
	    	      case FolVariableExpression(v) => FolVariableExpression(v);
	    	      case FolApplicationExpression(fun, arg) =>{
	    	        var newArg = arg;
	    	        for (p <- equalities)
				    {
				    	val origVarName = arg.asInstanceOf[FolVariableExpression].variable.name;
		    	    	if (p._2.equals(origVarName))
				    	{
				    	  newArg = FolVariableExpression(Variable(p._1));
			    	     //set type
				    	  val typ = p._1.charAt(0) match {
					      	case 'x' => "indv"
					        case 'e' => "evnt"
					        case 'p' => "prop"
					        case _ => "indv"
				    	  }
				    	  declarationTypes = declarationTypes.updated(0, typ)
				    	}
				    }	    	        
				    FolApplicationExpression (fun, newArg);
	    	      };
	    	      case _ => throw new Exception("wrong format of a declaration predicate");
	    	 }
	    	 FolApplicationExpression(newFun, newArg)

    	  }
    	  case _ => throw new Exception("wrong format of a declaration predicate");
    	}
    	declarationFol -> declarationTypes;
    }
    */
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
   	case FolAllExpression(variable, term) => FolAllExpression(removeEq(variable, eq), removeEq(term, eq));
   	case FolIfExpression(first, second) => FolIfExpression(removeEq(first, eq), removeEq(second, eq))
   	//case FolAllExpression(variable, term) => removeEq(term, eq);
   	//case FolIfExpression(first, second) => FolAndExpression(removeEq(first, eq), removeEq(second, eq))
   	
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
   	  val var1 = first match {
   	    case FolVariableExpression(variable) => variable.name
   	    case _ => throw new Exception ("Unrecongnized format, argumets of equality expression should be variables");
   	  }
   	 
   	  val var2 = second match {
   	    case FolVariableExpression(variable) => variable.name
   	    case _ => throw new Exception ("Unrecongnized format, argumets of equality expression should be variables");
   	  }
   	  
   	  //Both variables should be of the same type (both individuals or both events). 
   	  //Cases where variables types are different imply that the sentence is not parsed correctly. 
   	  //Such equality constraints are skipped because they are wrong and because they miss us the MLN 
   	  if (var1.charAt(0) == var2.charAt(0))
   	    return List((var1, var2));
   	  else return List();
   	}

   	//case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(v => Variable(namePrefix+v.name)))
   	case FolVariableExpression(v) => List();
    case FolApplicationExpression(fun, arg) => findEq(fun);
   }
   //return b;   
  }

}
