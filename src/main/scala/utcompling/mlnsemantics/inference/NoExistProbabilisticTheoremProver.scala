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
import scala.actors.Futures._
import scala.actors.threadpool.TimeoutException
import utcompling.mlnsemantics.inference.support.GoalExpression


class NoExistProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {
  	  
  object PermutTimesout extends Exception { }
  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Seq[Double] = {
    
    try 
    {
	    val newAssumptions:List[WeightedExpression[FolExpression]] =  
	      assumptions.map 
	      {
	          case GoalExpression(e, w) => 
	          {
	        	  var exp = e//goUniv(e, List(), List(), false);
	        	  GoalExpression(exp, w)
	          }
	          case a @ _ => a
	        }
	    
	    delegate.prove(
	      constants,
	      declarations,
	      evidence,
	      newAssumptions,
	      goal)
    }catch {
      case PermutTimesout => Seq(-4.0)
    }
    
  }
        
  //generate permutations
  private def permut[A](as: List[A], k: Int): List[List[A]] = 
    (List.fill(k)(as)).flatten.combinations(k).toList
    
   def runWithTimeout[T](timeoutMs: Long)(f: => T) : Option[T] = {
    awaitAll(timeoutMs, future(f)).head.asInstanceOf[Option[T]]
  }

  def runWithTimeout[T](timeoutMs: Long, default: T)(f: => T) : T = {
    runWithTimeout(timeoutMs)(f).getOrElse(default)
  }
/*    
  private def goExist(e: FolExpression, univVars: List[String], existVars: List[String], isNegated: Boolean): FolExpression =
  {
	  e match 
      {
      	case FolExistsExpression(v, term) => goExist(term, univVars, existVars:+v.name, isNegated)
      	case FolAllExpression(v, term) if (isNegated) => goExist(term, univVars, existVars:+v.name, isNegated)      	
        case _ => {
          if(univVars.size == 0)
          {
        	  goUniv(e, univVars, existVars, isNegated)
          }
          else
          {
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
	          
              
	          def genPermutes = {
					//println ("in")
	            permut(List.range (0, maxUnivConstListLen), univVars.size).foreach(p => {
					//println("inner");
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
	          }
				 println ("before permute");
	          //Sts.opts.timeout match  //regardless of the timeout parameter, timeout here is always inforced to 30 seconds 
	          //{
	          // case Some(t) => 
	              	val finish = runWithTimeout(30000, false) { genPermutes;  true }
	              	if(!finish)
	              		throw PermutTimesout
	          //  case _ => genPermutes; 
	          //}
				//genPermutes;
				println("after permute");
	          
	          extraUnivVars = extraUnivVars ++ existVars;  
	          newDeclarations = newDeclarations  ++ Map(skolemPred->skolemPredVarTypes)
	          var exp = (skolemPred->e).asInstanceOf[FolExpression];

	          //generate evidence
	          exp = goUniv(exp, univVars, existVars, isNegated)

	          existVars.foreach(v =>{
	        	  exp = FolAllExpression(Variable(v), exp);
        	  })

        	  exp
          }
        }
      }
  }
    
  private def goUniv(e: FolExpression, univVars: List[String], existVars: List[String], isNegated: Boolean): FolExpression = 
  {
      e match 
      {
      	case FolExistsExpression(v, term) => {
	      	 isNegated match {
	      	   case true => FolExistsExpression(v, goUniv(term, univVars:+v.name, existVars, isNegated))
	      	   case false => goExist(e, univVars, List(), isNegated)
	      	 }
      	} 
        case FolAllExpression(v, term) => {
        	isNegated match {
	      	   case true => goExist(e, univVars, List(), isNegated)
	      	   case false => FolAllExpression(v, goUniv(term, univVars:+v.name, existVars, isNegated))
	      	 }
        } 
        case FolNegatedExpression(term) => FolNegatedExpression(goUniv(term, univVars, existVars, !isNegated))
        case FolAndExpression(first, second) => FolAndExpression(goUniv(first, univVars, existVars, isNegated), 
        														goUniv(second, univVars, existVars, isNegated))
      	case FolOrExpression(first, second) => FolOrExpression(goUniv(first, univVars, existVars, isNegated),
      														goUniv(second, univVars, existVars, isNegated))
      	case FolIfExpression(first, second) => FolIfExpression(goUniv(first, univVars, existVars, !isNegated),
      														goUniv(second, univVars, existVars, isNegated))
      	case FolIffExpression(first, second) => throw new RuntimeException("not reachable")
        case FolEqualityExpression(first, second) => FolEqualityExpression(goUniv(first,univVars, existVars, isNegated)
        															, goUniv(second,univVars, existVars, isNegated))
        case FolAtom(pred, args @ _*) => e
        case _ => throw new RuntimeException("not reachable")
      }
  }
  * 
  */
}
