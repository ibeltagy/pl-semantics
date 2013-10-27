package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import utcompling.mlnsemantics.run.Sts
import scala.collection.mutable.MutableList
import utcompling.mlnsemantics.inference.support.GoalExpression

class HardAssumptionAsEvidenceProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  private var newConstants: Map[String, Set[String]] = null;
  private var extraEvid: List[FolExpression] = List();
  private var quantifiers: List[(String, Set[String])] = List();

  
  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Option[Double] = {
    
    newConstants = constants;//extra constants are added by skolemNew
    
    val newAssumptions:List[WeightedExpression[FolExpression]] = 
      assumptions
        .flatMap {
          case HardWeightedExpression(e) => {
        	  //outerExist = true;
              //val modifiedExp = getEvidTillFirstUniv(e, true);
        	  //quantifiers = List()        	  
        	  List(HardWeightedExpression(e))
          }
          case a @ _ => List(a)
        }
    
    delegate.prove(
      newConstants,
      declarations,
      evidence ++ extraEvid,  //toSet to remove duplicate evidences
      newAssumptions,
      goal)
    
  }
  private def addQuantifier(q:String, v:String) = 
  {
      if(quantifiers.size == 0)
  		  quantifiers :+ ((q, v));
  	  else
  	  {
  	    var lastQuant = quantifiers.last;
  	    quantifiers = quantifiers.slice(0, quantifiers.length-1)
  	    if(lastQuant._1 == q)
  	    	lastQuant = (lastQuant._1, lastQuant._2 + v) 
  	    else
  	    	lastQuant = (q, Set(v));
  	    quantifiers :+ lastQuant
  	  }
  }
  private def go(e: FolExpression): FolExpression = 
  {
      e match 
      {
      	case FolExistsExpression(v, term) => {
      	  addQuantifier("exist", v.name)
      	  if (quantifiers.size == 1)
      		  term //remove the outer existential quantifiers
      	  else
      		  FolExistsExpression(v, term);  //TODO: change it to univ and add the skolem term 
      	}
        case FolAllExpression(v, term) => {
          addQuantifier("univ", v.name)
          FolAllExpression(v, go(term)) //add the skolem predicate
        }
        case FolNegatedExpression(term) => throw new RuntimeException("TODO: not implemented yet")//TODO: FolNegatedExpression(term)
        case FolAndExpression(first, second) => FolAndExpression(go(first), go(second))
      	case FolOrExpression(first, second) => FolOrExpression(first, second)
      	case FolIfExpression(first, second) => FolIfExpression(first, second)
      	case FolIffExpression(first, second) => throw new RuntimeException("not reachable")      	
        case FolEqualityExpression(first, second) => FolEqualityExpression(first, second)
        case FolAtom(pred, args @ _*) => FolAtom(pred, args:_*)
		case FolVariableExpression(v) => throw new RuntimeException("not reachable")
        case _ => throw new RuntimeException("not reachable")
      }
  }
  private def getEvidTillFirstUniv(e: FolExpression, getEvid: Boolean): FolExpression = 
  {
	  if(!getEvid)
	    return e
      e match 
      {
      	case FolExistsExpression(v, term) => getEvidTillFirstUniv (term, getEvid);
        case FolAllExpression(v, term) => FolAllExpression(v, getEvidTillFirstUniv(term, getEvid))
        case FolNegatedExpression(term) => throw new RuntimeException("TODO: not implemented yet")//TODO: FolNegatedExpression(term)
        case FolAndExpression(first, second) => FolAndExpression(getEvidTillFirstUniv(first, getEvid), getEvidTillFirstUniv(second, getEvid))
      	case FolOrExpression(first, second) => FolOrExpression(first, second)
      	case FolIfExpression(first, second) => FolIfExpression(first, second)
      	case FolIffExpression(first, second) => throw new RuntimeException("not reachable")      	
        case FolEqualityExpression(first, second) => FolEqualityExpression(first, second)
        case FolAtom(pred, args @ _*) => FolAtom(pred, args:_*)
		case FolVariableExpression(v) => throw new RuntimeException("not reachable")
        case _ => throw new RuntimeException("not reachable")
      }
  }
  
    private def skolemNew(expr: FolExpression, skolemVars: List[Variable] = List(), isNegated:Boolean = false): FolExpression = 
    {
	  expr match {
	      case FolExistsExpression(variable, term) => {
	    	  if (isNegated)
	    		  FolExistsExpression(variable, skolemNew(term, skolemVars, isNegated))
	    	  else
	    		  skolemNew(term, skolemVars++List(variable), isNegated)
	      }
	      case FolAllExpression(variable, term) => {
	          if (!isNegated)
	        	  FolAllExpression(variable, skolemNew(term, skolemVars, isNegated))
	    	  else
	    		  skolemNew(term, skolemVars++List(variable), isNegated)
	      }
	      case FolNegatedExpression(term) => FolNegatedExpression (skolemNew(term, skolemVars, !isNegated))
	      case FolAndExpression(first, second) => FolAndExpression(skolemNew(first, skolemVars, isNegated), skolemNew(second, skolemVars, isNegated))
	      case FolOrExpression(first, second) => FolOrExpression(skolemNew(first, skolemVars, isNegated), skolemNew(second, skolemVars, isNegated))
	      case FolIfExpression(first, second) => FolIfExpression(skolemNew(first, skolemVars, !isNegated), skolemNew(second, skolemVars, isNegated))
	      case FolIffExpression(first, second) => throw new RuntimeException("FolIffExpression is not a valid expression")
	      case FolEqualityExpression(first, second) => FolEqualityExpression(skolemNew(first, skolemVars, isNegated), skolemNew(second, skolemVars, isNegated))	
	      case FolAtom(pred, args @ _*) => FolAtom.apply(pred, args.map(arg => skolemNew(arg, skolemVars)):_*);
		  case FolVariableExpression(v) => FolVariableExpression(skolemNew(v, skolemVars));
		  case _ => throw new RuntimeException(expr + " is not a valid expression")
	  }
	}
    private def skolemNew(v: Variable, skolemVars: List[Variable] ): Variable =
	{
	    if(skolemVars.contains(v))
	    {
	    	val newVarName = v.name+"_hPlus"
	    	val varType = v.name.substring(0, 2);
	    	newConstants += (varType -> (newConstants.apply(varType) + newVarName))
	    	Variable(newVarName)
	    }
	    else 
	    	v
	}
  
  
  /*
  private def renameVars(input: FolExpression): FolExpression = {
    return input
    input match {
    //case FolParseExpression(exps) => FolParseExpression( exps.map (e=> ( renameVars(e._1), e._2)) ) ;
   	//case FolExistsExpression(variable, term) => FolExistsExpression (variable, renameVars(term)) ;
   	//case FolAllExpression(variable, term) => FolAllExpression (variable, renameVars(term)) ;
   	//case FolNegatedExpression(term) => FolNegatedExpression(renameVars(term))
   	//case FolAndExpression(first, second) => FolAndExpression(renameVars(first), renameVars(second))
   	//case FolOrExpression(first, second) => FolOrExpression(renameVars(first), renameVars(second))   	
   	//case FolIfExpression(first, second) => FolIfExpression(renameVars(first), renameVars(second))
   	//case FolIffExpression(first, second) => FolIffExpression(renameVars(first), renameVars(second))
   	//case FolEqualityExpression(first, second) => FolEqualityExpression(renameVars(first), renameVars(second))

      //case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(v => Variable(namePrefix+v.name)))
   	//case FolVariableExpression(v) => FolVariableExpression(v)
    case FolAtom(pred, args @ _*) => {
       var newPredName = pred.name;
        newPredName = newPredName.replace("_dh", "_XXX");
        newPredName = newPredName.replace("_dt", "_YYY");
        newPredName = newPredName.replace("_XXX", "_dt");
        newPredName = newPredName.replace("_YYY", "_dh");
       FolAtom(Variable(newPredName), args:_*)
       
    }
    /*case FolApplicationExpression(fun, arg) =>{
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
      }*/
    case _ => input.visitStructured(renameVars, input.construct)
    }
  }
  * 
  */
}
