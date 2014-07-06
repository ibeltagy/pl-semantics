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
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import scala.actors.Futures._
import scala.actors.threadpool.TimeoutException


class AutoTypingPTP(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  private var allConstants: Map[String, Set[String]] = null;
  private var autoConst : scala.collection.mutable.Map[String, (Seq[String], scala.collection.mutable.Set[(String, Seq[String])])] = null; //predName#varIndx -> (type, H

  //private var arrows : scala.collection.mutable.Set[(Set[String], String)] = null; //predName#varIndx#varIndx -> predName#varIndx#varIndx 
  private var extraEvid: List[FolExpression] = null;
  private var quantifiedVars: scala.collection.mutable.Set[String] = null;
  private var isHardRule = true;
  //private var repeat = true;
  private var first = true;
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : Option[T] = {
          awaitAll(timeoutMs, future(f)).head.asInstanceOf[Option[T]]
  }

  def runWithTimeout[T](timeoutMs: Long, default: T)(f: => T) : T = {
      runWithTimeout(timeoutMs)(f).getOrElse(default)
  }
 
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
    extraEvid = List();
  	quantifiedVars = scala.collection.mutable.Set();
    autoConst = scala.collection.mutable.Map(); //predName#varIndx -> HX1, HX2 ....
    //arrows =  scala.collection.mutable.Set(); //an x -> y means all constants of x should be propagated to y
    //repeat = true;
	first = true;
    if(Sts.opts.negativeEvd && Sts.opts.task == "rte" && (Sts.opts.softLogicTool == "mln"|| Sts.opts.softLogicTool == "ss"))
    {
	    
	    declarations.foreach(d => {
	      d match {
	          case (FolAtom(pred, args @ _*), s) => { 
	            //require(args.length == 1 || args.length == 2)
	            autoConst += (pred.name ->((s, scala.collection.mutable.Set())));
	          }
	          case _ => throw new RuntimeException("Non-atomic declaration");
	      }
	    });
	        
	    evidence.foreach(e=>{
	      quantifiedVars.clear(); 
	      findConstVarsQuantif(e);
	    });

		def findApply = 
		{
		   //while (repeat)
		   //{
				//repeat = false;
		  
		   //1)collect constants, and propagate them according to the Text
		   first = true;
			assumptions.foreach 
			{
	          case HardWeightedExpression(e) => 
	          {
	            quantifiedVars.clear();
	            isHardRule = true;
	            val vars = findConstVarsQuantif(e);
	            //generate arrows from vars
	   			vars.foreach(rhsVar => 
	   			{
	   				vars.foreach(lhsVar =>
	   					propagate(Set(lhsVar), rhsVar)
	      			)
	      		})	            
	          }
             case SoftWeightedExpression(e, w) =>
             {
               quantifiedVars.clear();
               isHardRule = false;
               findArrowsIR (e)
             }
	          case _ => ;
			}
/*
			//2)apply infernece rules 
			assumptions.foreach 
			{			
	          case SoftWeightedExpression(e, w) => 
	          {
	            quantifiedVars.clear(); 
	            isHardRule = false; 
	            findArrowsIR (e)
	          }
	          case _ => ;
	        }
			
		   //3)propagate collected constants again according to the Text (if any)
			first = false;
			assumptions.foreach 
			{
	          case HardWeightedExpression(e) => 
	          {
	            quantifiedVars.clear();
	            isHardRule = true;
	            val vars = findConstVarsQuantif(e);
	            //generate arrows from vars
	   			vars.foreach(rhsVar => 
	   			{
	   				vars.foreach(lhsVar =>
	   					propagate(Set(lhsVar), rhsVar)
	      			)
	      		})	            
	           }
	          case _ => ;
			}
*/	    	 
			//}//Repeat
            genNegativeEvd(declarations);
		}
	
      val finish = runWithTimeout(3000, false) { findApply ;  true }

      if(!finish)
			return Seq(-5.0)
    }
    
    delegate.prove(
      constants,
      declarations,
      (evidence.toSet ++ extraEvid.toSet).toList,  //toSet to remove duplicate evidences
      assumptions,
      goal)
    
  }

  private def genNegativeEvd(declarations:Map[FolExpression, Seq[String]]) = 
  {    
    declarations.foreach(d => {
    d match 
    {
          case (FolAtom(pred, args @ _*), s) => {
            if(!pred.name.startsWith("skolem"))
            {	
              val t = autoConst(pred.name);
              assert(args.length == t._1.length)
              if (args.length == 1)
              {
            	  //Map[String, (Seq[String], scala.collection.mutable.Set[Seq[String]])]
            	  val possibleConst = t._2.map(_._2.head);
            	  allConstants(t._1.head).foreach(c=>
            	  {
            	      if(!possibleConst.contains(c))
            	      {
            	    	  var negEvd: FolExpression = FolVariableExpression(Variable(pred.name));
            	    	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c )));
            	    	  negEvd = FolNegatedExpression(negEvd);
            	    	  extraEvid = negEvd :: extraEvid;
            	      }
            	  })
            	  
              }else if (args.length == 2)
              {
            	  val possibleConst =  t._2.map(e=>(e._2.head, e._2.last));
            	  allConstants(t._1.head).foreach(c1=>
            	  {
            		  allConstants(t._1.last).foreach(c2=>
            		  {            	    
	            	      if(!possibleConst.contains((c1, c2)) && !( possibleConst.contains((c1, "any")) && possibleConst.contains(("any", c2))))
	            	      {
			            	  var negEvd: FolExpression = FolVariableExpression(Variable(pred.name));
			              	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c1 )));
			            	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c2 )));
			              	  negEvd = FolNegatedExpression(negEvd);
			              	  extraEvid = negEvd :: extraEvid;
	            	      }
            		  })
            	  })
              }
              else assert(false);
	         } //if not skolem
          } // case FolAtom
          case _ => throw new RuntimeException("Non-atomic declaration");
      }
    });
  }
/*  
  private def applyArrows() = 
  {
    var repeat = true;
    while(repeat)
    {
    	repeat = false;
    	arrows.foreach(arrow=>{
	      val rhsSetSize = autoConst(arrow._2)._2.size;
	      var constToPropagate = (arrow._1.map(v=>autoConst(v)._2)).reduceLeft(_&_)
	      if(! (constToPropagate -- autoConst(arrow._2)._2).isEmpty )
	        repeat = true;
	      autoConst(arrow._2)._2 ++= constToPropagate;
		})
    }
  }
*/
  private def propagate(lhs: Set[(String, Seq[String])], rhs: (String, Seq[String])):Any =
  {
	  object AllDone extends Exception { }
	  if (rhs._1.contains( "skolem"))//do not propagate to "skolem" predicates because they are already close-world
	    return 
	  assert(rhs._2.size < 3)
	  if((quantifiedVars & rhs._2.toSet).isEmpty) //intersection is empty, so all variables are not quantified
	     return //if both rhs variables are Constants
	  
	  var allExtraConst:Set[(String, Seq[String])] = null; 
		  
	  lhs.toList.sortBy( l => -l._2.length).foreach(lhsEntry=>{   //sorting to make predicates of multiple arguments come first. 
		  														//this is important for the intersection
		  try 
		  {
			  if(lhsEntry._1 == rhs._1) // do not add self-loops
			    throw AllDone
			  if (( lhsEntry._2.toSet & rhs._2.toSet ).isEmpty) //there is no variables overlap
			    throw AllDone
			  var lhsConstSet = autoConst(lhsEntry._1)._2;
			  lhsEntry._2.indices.foreach(lhsVarIdx=>{
			    val lhsVar = lhsEntry._2(lhsVarIdx);
			    if(!quantifiedVars.contains(lhsVar)) //Constant not variable
				  lhsConstSet = lhsConstSet.map(c=>(c._1, c._2.updated(lhsVarIdx, lhsVar)));
			  })

 
			  val lhsConstSetPropagated = lhsConstSet.flatMap(lhsConst=>
			  {
    			  //TODO: if lhsConst._1 does not match the conditions, return None
			      //if(isHardRule && lhsConst._1 == rhs._1)
			      if(isHardRule && lhsConst._1 == "TEXT") //do not propagate if in TEXT and source of the constant is   
			    	  								//a former propagation step in the TEXT
			      {
			        //println("CONST propagation canceled (TEXT)")
			        None
			      }
			      else if(!isHardRule && lhsConst._1 == "IR") //do not propagate is in IR and the const is resulting from 
			    	  										//a former application of IR
			      {
			        //println("CONST propagation canceled (IR)")
			        None
			      }			      
			      else
			      {
				      val lhsConstPropagated = rhs._2.map(rhsVar=>{
				        if(lhsEntry._2.contains(rhsVar))
				        {
				    	  val idx = lhsEntry._2.indexOf(rhsVar)
				    	  lhsConst._2(idx);
				        }
				        else "any";
				      })//end rhsConst
				      //Some((if(isHardRule) lhsEntry._1 else lhsConst._1, lhsConstPropagated));
				      Some((if(isHardRule) "TEXT" /*lhsEntry._1*/ else "IR", lhsConstPropagated));
			      }
			  }).toSet//end lhsConst
			  
			  if(allExtraConst ==  null)
			    allExtraConst = lhsConstSetPropagated;
			  else 
			  {
			    allExtraConst = allExtraConst.flatMap(e=>{
			      if(lhsConstSetPropagated.contains(e))
			        Some(e)
			      else
			      {
			        var found = false;
			        e._2.indices.foreach(idx=>{
			          if(lhsConstSetPropagated.unzip._2.contains(e._2.updated(idx, "any")))
			            found = true
			        })
			        if(found)
			          Some(e)
			        else
			          None
			      }
			  	})
			  }
			  
		  } catch {case AllDone =>}

	  })
	  if (allExtraConst != null)
	  {
/*	      if(lhs.size > 1)
	      {
	        //replace all "any" with actual constants
	        allExtraConst = allExtraConst.flatMap(extraC => 
	        {
	        	val cnt = extraC.count(_ == "any");
	        	if(cnt == 0)
	        		Some(extraC)
	            else if(cnt == extraC.size)
	            	None
	            else if(cnt > 1)
	            	throw new RuntimeException ("More than one 'any' is not implemented, and it should not happen");
	            else
	            {
	            	val i = extraC.indexOf("any");
	            	allExtraConst.flatMap(anyRep => {
	            	  if(anyRep(i) == "any")
	            	    None
	            	  else
	            	    Set(extraC.updated(i, anyRep(i)))
	            	})
	            }
	        })
	      }
	      * 
	      */
	      //if(! (allExtraConst -- autoConst(rhs._1)._2).isEmpty )
	      // repeat = true;
		  autoConst(rhs._1)._2 ++= allExtraConst;
	  }
  }
    
  private def findConstVarsQuantif(e: FolExpression): Set[(String, Seq[String])] =
  {
      e match 
      {
      	case FolExistsExpression(v, term) => quantifiedVars += v.name; findConstVarsQuantif(term);  
        case FolAllExpression(v, term) => quantifiedVars += v.name; findConstVarsQuantif(term);
        case FolEqualityExpression(first, second) => Set()
        case FolAtom(pred, args @ _*) =>
        	if(quantifiedVars.contains(args.head.name) && quantifiedVars.contains(args.last.name))
        	{
        	  None; // do nothing
        	}
        	else
        	{
        	  if(first) //add constants only in the first iteration
        	  {
				 //repeat = true;   //TODO: this should be uncommented but after reducing constatns from SetGoal, and fix AutoTyping
        	     autoConst(pred.name)._2  +=  (("CONST", args.map(arg=>{
        	      if(quantifiedVars.contains(arg.name)) "any" else arg.name
        	      })))
        	  }
        	}
        	Set((pred.name, args.map(_.name)));
       	case _ => e.visit(findConstVarsQuantif, (x:List[Set[(String, Seq[String])]])=> x.reduce( _ ++ _))
      }
  }
  
  //assume all inference rules are of the form  Univ LHS conjunctions => RHS conjunctions
  private def findArrowsIR(e: FolExpression) : Set[(String, Seq[String])] =
  {
	  e match 
      {
      	case FolAllExpression(v, term) => quantifiedVars += v.name; findArrowsIR(term);
      	case FolExistsExpression(v, term) => quantifiedVars += v.name; findArrowsIR(term);      	
      	case FolIfExpression(lhs, rhs) =>  {
      		val lhsVars = findArrowsIR(lhs);
      		val rhsVars = findArrowsIR(rhs)
   			rhsVars.foreach(rhsVar => propagate(lhsVars, rhsVar));
      		Set();
      	}
      	case FolAtom(pred, args @ _*) => 
      	  Set((pred.name, args.map(_.name)));
      	case _ => e.visit(findArrowsIR, (x:List[Set[(String, Seq[String])]])=> x.reduce( _ ++ _))
      }
  }
}
