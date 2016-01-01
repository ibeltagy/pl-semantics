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


class HardAssumptionAsEvidenceProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  private var newConstants: Map[String, Set[String]] = null;
  private var oldConstants: Map[String, Set[String]] = null;
  private var newDeclarations: Map[FolExpression, Seq[String]] = null;
  private var extraEvid: List[FolExpression] = List();
  private var skolemFunctionsCounter:Int = 0;
  //private var skolemConstCounter:Int = 0; 
  private var extraUnivVars: Set[String] = null;
  private var outerMost:Boolean = true;
  
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
    
    newConstants = constants;
    oldConstants = constants;
    newDeclarations = declarations;
    extraEvid = List();
  	skolemFunctionsCounter = 0;
  	extraUnivVars = null;
  	outerMost = true;
    try 
    {
	    val newAssumptions:List[WeightedExpression[FolExpression]] =  //if(Sts.opts.fixDCA == false) assumptions //if no-fixDCA, then do nothing
//	    else //fixDCA: handle skolem functions 
	      assumptions
	        .flatMap {
	          case HardWeightedExpression(e, w) => {
	        	  extraUnivVars = Set();
	        	  outerMost = true
	        	  var exp = goUniv(e, List(), List(), false);
	        	  if(Sts.opts.softLogicTool == "psl")
	        	  {
	        	    conjToEvd(exp);
	        	    List()
	        	  }
	        	  else
	        	    List(HardWeightedExpression(exp, w))
	          }
	          case a @ _ => List(a)
	        }
		 //println("done skolem")

		//A default constant is added to constants types with empty constats list
		val newConstantsCopy = newConstants;
		newConstantsCopy.foreach {
			case (name, tokens) => {
				if (tokens.size == 0)
					addConst(name + "_default")
			}
		}

	    delegate.prove(
	      newConstants,
	      newDeclarations,
	      (evidence.toSet ++ extraEvid.toSet).toList,  //toSet to remove duplicate evidences
	      newAssumptions,
	      goal)
    }catch {
      case PermutTimesout => Seq(-4.0)
    }
  }
  
  private def conjToEvd(e: FolExpression): Any = 
  {
      e match 
      {
        case FolAtom(pred, args @ _*) => extraEvid = e :: extraEvid;
        case _ => e.visit(conjToEvd, (x:List[Any])=> 0)
      }
  }
      
      
  private def addConst(varName: String) =
		newConstants += (varName.substring(0, 2) -> (newConstants.apply(varName.substring(0, 2)) + varName))
  
    
  private def goExist(e: FolExpression, univVars: List[String], existVars: List[String], isNegated: Boolean): FolExpression =
  {
	  e match 
      {
      	case FolExistsExpression(v, term) => assert(!isNegated); goExist(term, univVars, existVars:+v.name, isNegated)
      	case FolAllExpression(v, term) if (isNegated) => goExist(term, univVars, existVars:+v.name, isNegated)      	
        case _ => {
          if(univVars.size == 0)
          {
        	  //existVars.foreach(addConst(_))
        	  goUniv(e, univVars, existVars, isNegated)
          }
          else
          {
        	  outerMost = false;
	          skolemFunctionsCounter = skolemFunctionsCounter + 1;
	          val skolemPredName = "skolem_"+skolemFunctionsCounter + "_" + univVars.size;
	          var skolemPred : FolExpression = FolVariableExpression(Variable(skolemPredName));
	          var skolemPredVarTypes:List[String] = List();
	          var univConst: List[List[String]] = List();
	          var maxUnivConstListLen:Int = 0;
	          univVars.foreach(univVar =>{
	            skolemPred = FolApplicationExpression(skolemPred, FolVariableExpression(Variable(univVar)));
	            skolemPredVarTypes = skolemPredVarTypes  ++ List((univVar.substring(0, 2)))
	            val constList = oldConstants .get(univVar.substring(0, 2)).get;
	            univConst = univConst ++ List(constList.toList)
	            maxUnivConstListLen = scala.math.max(maxUnivConstListLen, constList.size);
	            //println( oldConstants .get(univVar.substring(0, 2)))
	          })
	          existVars.foreach(existVar =>{
	            skolemPred = FolApplicationExpression(skolemPred, FolVariableExpression(Variable(existVar)));
	            skolemPredVarTypes = skolemPredVarTypes  ++ List((existVar.substring(0, 2)))
	          })
	          
	          /*
	          println ("before permute");
	          //Sts.opts.timeout match  //regardless of the timeout parameter, timeout here is always inforced to 30 seconds 
	          //{
	          // case Some(t) => 
	              	val finish = runWithTimeout(3000, false) { 
	              		 val evidAndConst = genPermutes (maxUnivConstListLen, univVars.size, skolemPredName: String, univConst, existVars);
	              		 extraEvid = extraEvid  ++ evidAndConst._1;
	              		 evidAndConst._2.foreach(x => addConst(x));
	              		 true;
	              	}
	              	if(!finish)
	              		throw PermutTimesout
	          //  case _ => genPermutes; 
	          //}
				//genPermutes;
				println("after permute");
	           */
			  
	          extraUnivVars = extraUnivVars ++ existVars;  
	          newDeclarations = newDeclarations  ++ Map(skolemPred->skolemPredVarTypes)
	          var exp = if (isNegated)
	        	  			(-(skolemPred -> -e)).asInstanceOf[FolExpression];
	        	  		else 
	        	  			(skolemPred->e).asInstanceOf[FolExpression];

	          //generate evidence
	          exp = goUniv(exp, univVars, existVars, isNegated)

	          existVars.foreach(v =>{
	            if(isNegated)
	            	exp = FolExistsExpression(Variable(v), exp);
	            else
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
        case FolNegatedExpression(term) =>  outerMost = false;
        			FolNegatedExpression(goUniv(term, univVars, existVars, !isNegated))
        case FolAndExpression(first, second) => 
				{
					//this is actually useless, but I will keep it
					def isGroundAtom (e_ : FolExpression, univVars_ : List[String], existVars_ : List[String], isNegated_ : Boolean): Boolean = 
					{
						if (isNegated_)
							return false;
						if (univVars_.length > 0) 
							return false;
						if (!outerMost)
							return false;
						e_ match 
						{
							case FolAtom(pred, args @ _*) => 
								//all variables of the atom are existentially quantified 
								if( (existVars_.toSet & args.map(_.name).toSet ).size  != args.toSet.size )
									throw new RuntimeException ("args: " + args + ", atom: " + e_ + " not in existVars: " + existVars_ + " and not in univVars: " + univVars_)

								extraEvid = e_ :: extraEvid;
								return false
							case _ => return false
						}
					}
					
					if ( isGroundAtom (first, univVars, existVars, isNegated) )
						goUniv(second, univVars, existVars, isNegated)
					else if ( isGroundAtom (second, univVars, existVars, isNegated) )
						goUniv(first, univVars, existVars, isNegated)
					else
						FolAndExpression(goUniv(first, univVars, existVars, isNegated), 
							goUniv(second, univVars, existVars, isNegated))
				}
      	case FolOrExpression(first, second) => outerMost = false;
      				FolOrExpression(goUniv(first, univVars, existVars, isNegated),
      					goUniv(second, univVars, existVars, isNegated))
      	case FolIfExpression(first, second) => outerMost = false;
      				FolIfExpression(goUniv(first, univVars, existVars, !isNegated),
      					goUniv(second, univVars, existVars, isNegated))
      	case FolIffExpression(first, second) => throw new RuntimeException("not reachable")
        case FolEqualityExpression(first, second) => FolEqualityExpression(goUniv(first,univVars, existVars, isNegated)
        															, goUniv(second,univVars, existVars, isNegated))
        case FolAtom(pred, args @ _*) => //FolAtom(pred, args:_*);
		        FolAtom(pred, args.map(arg=>{
		          goUniv(arg, univVars, existVars, isNegated)
		        }):_*)
		    case FolVariableExpression(v) => //FolVariableExpression(v) 
				FolVariableExpression(goUniv(v, univVars, existVars, isNegated));
        case _ => throw new RuntimeException("not reachable")
      }
  }
  private def goUniv(v: Variable, univVars: List[String], existVars: List[String], isNegated: Boolean): Variable =
  {
    if (univVars.size == 0 && existVars.contains(v.name))
    {
      addConst(v.name)
      /*val newVarName = v.name
      val varType = v.name.substring(0, 2);
      newConstants += (varType -> (newConstants.apply(varType) + newVarName))
      * 
      */
    }
    v
  }

}

object HardAssumptionAsEvidenceProbabilisticTheoremProver
{
    var skolemConstCounter = 0
   
	def getInputVarCount (skolemPredName: String) : Int =
	{
	  assert(skolemPredName.startsWith("skolem_"))
	  assert( skolemPredName.count(_ == '_') == 2)
	  val lastIndexOfUndescore = skolemPredName.lastIndexOf("_");
	  return Integer.parseInt(skolemPredName.substring(lastIndexOfUndescore+1));
	}
        
    def genPermutes (listLen:Int, listsCount:Int, predName: String, constMatrix: List[List[String]], outVar :List[String], generatedBefore:Set[List[String]]) :
    (List[FolExpression], List[String] ) = 
    {
    	var generatedEvid: List[FolExpression] = List();  
    	var extraConst:List[String] = List()
      	permut(List.range (0, listLen), listsCount).foreach(c => { c.permutations.foreach( p=> {
		  var skolemEvd: FolExpression = FolVariableExpression(Variable(predName));
		  object AllDone extends Exception { }
		  try
		  {
	    	 var pConst:List[String] = List(); 
		  	 for(i <- 0 to p.length-1)
	    	 {
	    	    val idx = p.apply(i)
	    	    val constListForI = constMatrix.apply(i);
	    	    if (constListForI.size <= idx)
	    	      throw AllDone;
	    	    pConst = pConst :+ constListForI.apply(idx);
	    		skolemEvd = FolApplicationExpression(skolemEvd, FolVariableExpression(Variable(constListForI.apply(idx))));
	    	 }
		  	 if (!generatedBefore.contains(pConst))
		  	 {
	    	    outVar.foreach(existVar =>{
	    	      val newConstName = existVar +"_" + HardAssumptionAsEvidenceProbabilisticTheoremProver.skolemConstCounter;
	    	      HardAssumptionAsEvidenceProbabilisticTheoremProver.skolemConstCounter = 1 + HardAssumptionAsEvidenceProbabilisticTheoremProver.skolemConstCounter;
	    	      extraConst = newConstName :: extraConst; 
	              skolemEvd = FolApplicationExpression(skolemEvd, FolVariableExpression(Variable(newConstName)));
	            })
	            generatedEvid = skolemEvd :: generatedEvid;
		  	 }
		  }catch{
		      case AllDone =>//do nothing
		  }
	    }) /*END P*/ }) /*END C*/
	    (generatedEvid, extraConst)
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
}
