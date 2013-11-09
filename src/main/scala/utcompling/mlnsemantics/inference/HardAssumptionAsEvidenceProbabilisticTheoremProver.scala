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
  private var oldConstants: Map[String, Set[String]] = null;
  private var newDeclarations: Map[FolExpression, Seq[String]] = null;
  private var extraEvid: List[FolExpression] = List();
  private var skolemFunctionsCounter:Int = 0;
  private var skolemConstCounter:Int = 0; 
  private var extraUnivVars: Set[String] = null;
  
  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Option[Double] = {
    
    newConstants = constants;
    oldConstants = constants;
    newDeclarations = declarations;
    extraEvid = List();
  	skolemFunctionsCounter = 0;
  	skolemConstCounter = 0; 
  	extraUnivVars = null;

    
    val newAssumptions:List[WeightedExpression[FolExpression]] =  if(Sts.opts.fixDCA == false) assumptions //if no-fixDCA, then do nothing
    else //fixDCA: handle skolem functions 
      assumptions
        .flatMap {
          case HardWeightedExpression(e) => {
        	  extraUnivVars = Set();
        	  var exp = goUniv(e, List())
        	  extraUnivVars.foreach(univVar =>{
        	    exp = FolAllExpression(Variable(univVar), exp);
        	  })
        	  List(HardWeightedExpression(exp))
          }
          case a @ _ => List(a)
        }
    
    delegate.prove(
      newConstants,
      newDeclarations,
      evidence ++ extraEvid,  //toSet to remove duplicate evidences
      newAssumptions,
      goal)
    
  }
  
  private def addConst(varName: String) =
		newConstants += (varName.substring(0, 2) -> (newConstants.apply(varName.substring(0, 2)) + varName))
  
  //generate permutations
  private def permut[A](as: List[A], k: Int): List[List[A]] = 
    (List.fill(k)(as)).flatten.combinations(k).toList
    
  private def goExist(e: FolExpression, univVars: List[String], existVars:List[String] ): FolExpression =
  {
	  e match 
      {
      	case FolExistsExpression(v, term) => goExist(term, univVars, existVars ++ List(v.name))
        case _ => {
          skolemFunctionsCounter = skolemFunctionsCounter + 1;
          var skolemPred : FolExpression = FolVariableExpression(Variable("skolem_"+skolemFunctionsCounter));
          var skolemPredVarTypes:List[String] = List();
          var univConst: List[List[String]] = List();
          var maxUnivConstListLen:Int = 0;
          univVars.foreach(univVar =>{
            skolemPred = FolApplicationExpression(skolemPred, FolVariableExpression(Variable(univVar)));
            skolemPredVarTypes = skolemPredVarTypes  ++ List((univVar.substring(0, 2)))
            val constList = oldConstants .get(univVar.substring(0, 2)).get;
            univConst = univConst ++ List(constList.toList)
            maxUnivConstListLen = scala.math.max(maxUnivConstListLen, constList.size);
            println( oldConstants .get(univVar.substring(0, 2)))
          })
          existVars.foreach(existVar =>{
            skolemPred = FolApplicationExpression(skolemPred, FolVariableExpression(Variable(existVar)));
            skolemPredVarTypes = skolemPredVarTypes  ++ List((existVar.substring(0, 2)))
          })
          
          permut(List.range (0, maxUnivConstListLen), univVars.size).foreach(p => {
        	  var skolemEvd: FolExpression = FolVariableExpression(Variable("skolem_"+skolemFunctionsCounter));
        	  object AllDone extends Exception { }
        	  try
        	  {
	        	  for(i <- 0 to p.length-1)
	        	  {
	        	    val idx = p.apply(i)
	        	    val constListForI = univConst.apply(i);
	        	    if (constListForI.size <= idx)
	        	      throw AllDone;
	        		skolemEvd = FolApplicationExpression(skolemEvd, FolVariableExpression(Variable(constListForI.apply(idx))));        	    
	        	  }
	        	  existVars.foreach(existVar =>{
	        	    val newConstName = existVar +"_" + skolemConstCounter;
	        	    skolemConstCounter = skolemConstCounter + 1;
	        	    addConst(newConstName);
		            skolemEvd = FolApplicationExpression(skolemEvd, FolVariableExpression(Variable(newConstName)));
		          })
		          extraEvid = skolemEvd :: extraEvid;
        	  }catch{
        	      case AllDone =>//do nothing
        	  }
          })
          extraUnivVars = extraUnivVars ++ existVars;  
          newDeclarations = newDeclarations  ++ Map(skolemPred->skolemPredVarTypes)
          //generate evidence
          goUniv(skolemPred->e, univVars)
        }
      }
  }
    
    
  private def goUniv(e: FolExpression, univVars: List[String]): FolExpression = 
  {
      e match 
      {
      	case FolExistsExpression(v, term) => goExist(e, univVars, List()) 
        case FolAllExpression(v, term) => FolAllExpression(v, goUniv(term, univVars ++ List((v.name)))) 
        case FolNegatedExpression(term) => FolNegatedExpression(goUniv(term, univVars))
        case FolAndExpression(first, second) => FolAndExpression(goUniv(first, univVars), goUniv(second, univVars))
      	case FolOrExpression(first, second) => FolOrExpression(goUniv(first, univVars), goUniv(second, univVars))
      	case FolIfExpression(first, second) => FolIfExpression(goUniv(first, univVars), goUniv(second, univVars))
      	case FolIffExpression(first, second) => throw new RuntimeException("not reachable")
        case FolEqualityExpression(first, second) => FolEqualityExpression(first, second)
        case FolAtom(pred, args @ _*) => FolAtom(pred, args:_*)
		case FolVariableExpression(v) => throw new RuntimeException("not reachable")
        case _ => throw new RuntimeException("not reachable")
      }
  }
}
