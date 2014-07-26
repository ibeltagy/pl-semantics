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
import org.joda.time.DateTimeUtils
import org.joda.time.Duration


class NoExistProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {
  	  
  object PermutTimesout extends Exception { }
  private var allConstants: Map[String, Set[String]] = null;
  private var allEvidence: List[FolExpression] = null;
  private val trueFolExp:FolExpression = FolVariableExpression(Variable("true"));
  private val falseFolExp:FolExpression = FolVariableExpression(Variable("false"));  
  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Seq[Double] = {
    
    allConstants = constants;
    allEvidence = evidence;
    val startTime = DateTimeUtils.currentTimeMillis()
    val res = try 
    {
		  var constCount :Integer = 0;
		  allConstants.forall(c=>{
				println(c);
				constCount = constCount + c._2.size
				true
		  })
		  println("const count = " + constCount);

        val newAssumptions:List[WeightedExpression[FolExpression]] = 
        if(! (Sts.opts.groundExist 	&& Sts.opts.task == "rte" && Sts.opts.negativeEvd/*this is only useful if there are negative evd*/ 
												&& constCount > 1/*in case of no constants in the domain, skip this step*/) ) 
          assumptions
	    else
	      assumptions.map 
	      {
	          case GoalExpression(e, w) => 
	          {
	        	  if(e == trueFolExp)
	        		  return Seq(1.0)
	        	  val expVarRenamed = rename(e); //all variables have to be renamed to avoid conflict between variables and constants. 
	        	  var exp = goUniv(expVarRenamed, List(), List(), false);
	        	  exp = replaceSimplify(exp, Set());
	        	  getBoolean3(exp) match 
			      {
			      	case 't' => return Seq(1.0);
			      	case 'f' => return Seq(0.0);
			      	case 'd' => GoalExpression(exp, w)
		      	  }
	          }
	          case a @ _ => a
	      }
	    val beforeInfTime = DateTimeUtils.currentTimeMillis();
	    val totalTimeout = Sts.opts.timeout;
	    Sts.opts.timeout = Some(Sts.opts.timeout.get - (beforeInfTime - startTime))
	    
	    var infRes =  Seq(-6.0);
	    
	    if(Sts.opts.timeout.get > 0)
	    {
		    println("#Run infer for# " + Sts.opts.timeout.get)
	        infRes = delegate.prove(
		      constants,
		      declarations,
		      evidence,
		      newAssumptions,
		      goal)
	    }
	    Sts.opts.timeout = totalTimeout;
	    
	    infRes;
    }catch {
      case PermutTimesout => Seq(-6.0)
	  //case _  => Seq(-5.0)
    }
    
    var endTime = DateTimeUtils.currentTimeMillis()
    if(res.head < 0 )
    	println ("#Time# (timeout) (" + res.head + ") "+(endTime - startTime) )
    else 
        println ("#Time# (good) (" + res.head + ") "+(endTime - startTime)  )
    res
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
    
  var varToReplace: List[String] = null;
  var replaceWith: List[String]  = null;
  
  private def goExist(e: FolExpression, univVars: List[String], existVars: List[String], isNegated: Boolean): FolExpression =
  {
	  e match 
      {
      	case FolExistsExpression(v, term) => goExist(term, univVars, existVars:+v.name, isNegated)
      	case FolAllExpression(v, term) if (isNegated) => goExist(term, univVars, existVars:+v.name, isNegated)      	
        case _ => {
          var existConst: List[List[String]] = List();
          var maxUnivConstListLen:Int = 0;
          existVars.foreach(existVar =>{
            val constList = allConstants.get(existVar.substring(0, 2)).get;
            existConst = existConst ++ List(constList.toList)
            maxUnivConstListLen = scala.math.max(maxUnivConstListLen, constList.size);
            println( allConstants .get(existVar.substring(0, 2)))
          })
          
          var newExp: FolExpression = null;
          def genPermutes = {
            permut(List.range (0, maxUnivConstListLen), existVars.size).foreach(p => {
        	  object AllDone extends Exception { }
        	  try
        	  {
	        	  replaceWith = List();
        	      for(i <- 0 to p.length-1)
	        	  {
	        	    val idx = p.apply(i)
	        	    val constListForI = existConst.apply(i);
	        	    if (constListForI.size <= idx)
	        	      throw AllDone;
	        	    replaceWith = replaceWith :+ constListForI.apply(idx);
	        	  }
        	      varToReplace = existVars;
        	      val oneNewExp = replaceSimplify(e, univVars.toSet);
        	      if(newExp == null)
        	        newExp = oneNewExp;
        	      else if(isNegated)
					{
        	           //newExp = FolAndExpression(newExp, oneNewExp);
					     newExp = (getBoolean3(newExp),getBoolean3(oneNewExp)) match
						  {
							 case ('t', 't') => trueFolExp
							 case ('t', 'f') => falseFolExp
							 case ('t', 'd') => oneNewExp
							 case ('f', 't') => falseFolExp
							 case ('f', 'f') => falseFolExp
							 case ('f', 'd') => falseFolExp
							 case ('d', 't') => newExp
							 case ('d', 'f') => falseFolExp
							 case ('d', 'd') => FolAndExpression(newExp, oneNewExp);
						  }
					}
        	      else
					{
        	           //newExp = FolOrExpression(newExp, oneNewExp);
						  newExp = (getBoolean3(newExp),getBoolean3(oneNewExp)) match
						  {
							 case ('t', 't') => trueFolExp
							 case ('t', 'f') => trueFolExp
							 case ('t', 'd') => trueFolExp
							 case ('f', 't') => trueFolExp
							 case ('f', 'f') => falseFolExp
							 case ('f', 'd') => oneNewExp
							 case ('d', 't') => trueFolExp
							 case ('d', 'f') => newExp
							 case ('d', 'd') => FolOrExpression(newExp, oneNewExp);
						  }

					}
        	      //println(oneNewExp);
        	  }catch{
        	      case AllDone =>//do nothing
        	  }
            })
          }
	      val finish = runWithTimeout(Sts.opts.timeout.get, false) { genPermutes;  true }
	      if(!finish)
	    	  throw PermutTimesout

	       replaceWith = List();
	       varToReplace = List();
	       newExp = replaceSimplify(newExp, univVars.toSet);
	       getBoolean3(newExp) match 
	       {
      		  case 'd' => goUniv(newExp, univVars, existVars, isNegated)
      		  case _ => newExp
      	 }

        }
      }
  }
  private def replaceSimplify(v: Variable, quantifiers: Set[String]): Variable =
  {
    if(!quantifiers.contains(v.name) && varToReplace.contains(v.name))
    {
      val i = varToReplace.indexOf(v.name);
      Variable(replaceWith(i))
    }
    else 
      v
  }
  private def getBoolean3(e:FolExpression) : Char = 
  {
    if(e == trueFolExp)
    	't';
    else if(e == falseFolExp)
    	'f';
    else 
    	'd';
  }
  
  private def rename(v: Variable): Variable =
	Variable(v.name + "_q")	  
    
  private def rename(e: FolExpression): FolExpression =
  {
    e match {
    	case FolExistsExpression(v, term) => FolExistsExpression(rename(v), rename(term))	
    	case FolAllExpression(v, term) => FolAllExpression(rename(v), rename(term))
    	case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(rename(_)) :_ * )	
    	case FolVariableExpression(v) => FolVariableExpression(rename(v)) 
    	case _=>e.visitStructured(rename, e.construct)
    }
  }
  private def replaceSimplify(e: FolExpression, quantifiers: Set[String]): FolExpression =
  {
      e match 
      {
      	case FolExistsExpression(v, term) => 
            val mTerm = replaceSimplify(term, quantifiers + v.name)
      		getBoolean3(mTerm)  match 
      		{
      		  case 't' => trueFolExp
      		  case 'f' => falseFolExp
      		  case 'd' => FolExistsExpression(v, mTerm);
      		} 
        case FolAllExpression(v, term) =>
            val mTerm = replaceSimplify(term, quantifiers + v.name)
      		getBoolean3(mTerm)  match 
      		{
      		  case 't' => trueFolExp
      		  case 'f' => falseFolExp
      		  case 'd' => FolAllExpression(v, mTerm);
      		}
        case FolNegatedExpression(term) => 
            val mTerm = replaceSimplify(term, quantifiers) 
      		getBoolean3(mTerm)  match 
      		{
      		  case 't' => falseFolExp
      		  case 'f' => trueFolExp
      		  case 'd' => FolNegatedExpression(mTerm);
      		}            
        case FolAndExpression(first, second) =>
            val mFirst = replaceSimplify(first, quantifiers) 
      		val mSecond = replaceSimplify(second, quantifiers)
      		(getBoolean3(mFirst),getBoolean3(mSecond)) match 
      		{
      		  case ('t', 't') => trueFolExp
      		  case ('t', 'f') => falseFolExp
      		  case ('t', 'd') => mSecond
      		  case ('f', 't') => falseFolExp
      		  case ('f', 'f') => falseFolExp
      		  case ('f', 'd') => falseFolExp
      		  case ('d', 't') => mFirst
      		  case ('d', 'f') => falseFolExp
      		  case ('d', 'd') => FolAndExpression(mFirst, mSecond);      		    
      		}
      	case FolOrExpression(first, second) =>
      	   val mFirst = replaceSimplify(first, quantifiers) 
      		val mSecond = replaceSimplify(second, quantifiers)
      		(getBoolean3(mFirst),getBoolean3(mSecond)) match 
      		{
      		  case ('t', 't') => trueFolExp
      		  case ('t', 'f') => trueFolExp
      		  case ('t', 'd') => trueFolExp
      		  case ('f', 't') => trueFolExp
      		  case ('f', 'f') => falseFolExp
      		  case ('f', 'd') => mSecond
      		  case ('d', 't') => trueFolExp
      		  case ('d', 'f') => mFirst
      		  case ('d', 'd') => FolOrExpression(mFirst, mSecond);      		    
      		}
      	case FolIfExpression(first, second) => 
      		val mFirst = replaceSimplify(first, quantifiers) 
      		val mSecond = replaceSimplify(second, quantifiers)
      		(getBoolean3(mFirst),getBoolean3(mSecond)) match 
      		{
      		  case ('t', 't') => trueFolExp
      		  case ('t', 'f') => falseFolExp
      		  case ('t', 'd') => mSecond
      		  case ('f', 't') => trueFolExp
      		  case ('f', 'f') => trueFolExp
      		  case ('f', 'd') => trueFolExp
      		  case ('d', 't') => trueFolExp
      		  case ('d', 'f') => FolNegatedExpression(mFirst)
      		  case ('d', 'd') => FolIfExpression(mFirst, mSecond);      		    
      		}
      		
      	case FolIffExpression(first, second) => throw new RuntimeException("not reachable")
        case FolEqualityExpression(first, second) => FolEqualityExpression(replaceSimplify(first, quantifiers), 
        													replaceSimplify(second, quantifiers));
        case FolAtom(pred, args @ _*) => 
          val changedArgs = args.map(replaceSimplify(_, quantifiers))
          val changedE = FolAtom(pred,changedArgs:_*);
          
          if( (changedArgs.map(_.name).toSet & quantifiers).isEmpty )
          { //all constants
            if(allEvidence.contains(changedE))
              trueFolExp
            else if (allEvidence.contains(FolNegatedExpression(changedE)))
              falseFolExp
            else 
              changedE
          }else 
            changedE //some variables
        
        case FolVariableExpression(v) => FolVariableExpression(replaceSimplify(v, quantifiers));

        case _ => println (e); throw new RuntimeException("not reachable")
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
      	case FolIffExpression(first, second) => goUniv(FolAndExpression(FolIfExpression(first, second), FolIfExpression(second, first)), univVars, existVars, isNegated)
        case FolEqualityExpression(first, second) => e
        case FolAtom(pred, args @ _*) => e
        case _ => throw new RuntimeException("not reachable")
      }
  }
}
