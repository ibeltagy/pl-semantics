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

class AutoTypingPTP(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  private var allConstants: Map[String, Set[String]] = null;
  private var autoConst : scala.collection.mutable.Map[String, (String, String, scala.collection.mutable.Set[(String, String)])] = null; //predName#varIndx -> (type, H

  //private var arrows : scala.collection.mutable.Set[(Set[String], String)] = null; //predName#varIndx#varIndx -> predName#varIndx#varIndx 
  private var extraEvid: List[FolExpression] = null;
  private var quantifiedVars: scala.collection.mutable.Set[String] = null;
  private var repeat = true;
  private var first = true;
  
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
    repeat = true;
	first = true;
    
    if(Sts.opts.negativeEvd && Sts.opts.task == "rte" && (Sts.opts.softLogicTool == "mln"|| Sts.opts.softLogicTool == "ss"))
    {
	    
	    declarations.foreach(d => {
	      d match {
	          case (FolAtom(pred, args @ _*), s) => { 
	            require(args.length == 1 || args.length == 2)
	            autoConst += ("%s#%s#%s".format(pred.name, args.indices.head, args.indices.last)->((s.head, s.last, scala.collection.mutable.Set())));
	          }
	          case _ => throw new RuntimeException("Non-atomic declaration");
	      }
	    });
	        
	    evidence.foreach(e=>{
	      quantifiedVars.clear(); 
	      findConstVarsQuantif(e);
	    });
		while (repeat)
		{
			repeat = false;
			assumptions.foreach{
	          case HardWeightedExpression(e) => {
	            quantifiedVars.clear(); 
	            val vars = findConstVarsQuantif(e);
	            //generate arrows from vars
	   			vars.foreach(rhsVar => {
	   				vars.foreach(lhsVar =>
	   					propagate(Set(lhsVar), rhsVar)
	      			)
	      		})	            
	          }
	          case SoftWeightedExpression(e, w) => quantifiedVars.clear(); findArrowsIR (e)
	          case _ => ;
	        }
	    	first = false;
		}//Repeat
	    
	    //applyArrows();
	    
	    genNegativeEvd(declarations);
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
              val t = autoConst("%s#%s#%s".format(pred.name, args.indices.head, args.indices.last));
              if (args.length == 1)
              {
            	  assert(t._1 == t._2);
            	  val(c1, c2) = t._3.unzip;
            	  val possibleConst = c1; 
            	  allConstants(t._1).foreach(c=>
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
            	  val possibleConst =  t._3;
            	  allConstants(t._1).foreach(c1=>
            	  {
            		  allConstants(t._2).foreach(c2=>
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
  private def propagate(lhs: Set[(String, String, String)], rhs: (String, String, String)):Any =
  {
	  object AllDone extends Exception { }
	  if (rhs._3.contains( "skolem"))//do not propagate to "skolem" predicates because they are already close-world
	    return 
	  if(!quantifiedVars.contains(rhs._1) && !quantifiedVars.contains(rhs._2))
	     return //if both rhs variables are Constants
	  
	  var allExtraConst:Set[(String, String)] = null; 
		  
	  lhs.foreach(lhsVar=>{
		  try 
		  {
			  if(lhsVar._3 == rhs._3) // do not add self-loops
			    throw AllDone
			  if ((Set(lhsVar._1, lhsVar._2) & Set(rhs._1, rhs._2).toSet ).isEmpty) //there is variables overlap
			    throw AllDone
			  var lhsConst = autoConst(lhsVar._3);
			  //assert(lhsConst._1 == lhsVar._1)
			  //assert(lhsConst._2 == lhsVar._2)
			  var lhsConstSet = lhsConst._3; 

			  if(!quantifiedVars.contains(lhsVar._1)) //Constant not variable
				  lhsConstSet = lhsConstSet.map(c=>(lhsVar._1, c._2));
			  if(!quantifiedVars.contains(lhsVar._2)) //Constant not variable
				  lhsConstSet = lhsConstSet.map(c=>(c._1, lhsVar._2));
			  val lhsConstSetUnzip = lhsConstSet.toList.unzip
			  var col1 : List[String] = List();
			  var col2 : List[String] = List();
			  if(rhs._1 == lhsVar._1)
			  {
			    col1 = lhsConstSetUnzip._1
			  }
			  else if(rhs._1 == lhsVar._2)
			  {
			    col1 = lhsConstSetUnzip._2
			  }
			  
			  if(rhs._2 == lhsVar._1)
			  {
			    col2 = lhsConstSetUnzip._1
			  }
			  else if(rhs._2 == lhsVar._2)
			  {
			    col2 = lhsConstSetUnzip._2
			  }
			  //assert(!(col1.isEmpty&&col2.isEmpty))
			  if(col1.isEmpty)
			    col1 = List.fill(col2.size)("any");
			  if(col2.isEmpty)
			    col2 = List.fill(col1.size)("any");
			  
			  val extraConst = (col1 zip col2).toSet
			  if(allExtraConst ==  null)
			    allExtraConst = extraConst;
			  else allExtraConst = (extraConst & allExtraConst);
			  
		  } catch {case AllDone =>}

	  })
	  if (allExtraConst != null)
	  {
	      if(! (allExtraConst -- autoConst(rhs._3)._3).isEmpty )
	        repeat = true;
		  autoConst(rhs._3)._3 ++= allExtraConst;
	  }
  }
    
  private def findConstVarsQuantif(e: FolExpression): Set[(String, String, String)] =
  {
      e match 
      {
      	case FolExistsExpression(v, term) => quantifiedVars += v.name; findConstVarsQuantif(term);  
        case FolAllExpression(v, term) => quantifiedVars += v.name; findConstVarsQuantif(term);
        case FolAtom(pred, args @ _*) =>
        	if(quantifiedVars.contains(args.head.name) && quantifiedVars.contains(args.last.name))
        	{
        	  None; // do nothing
        	}
        	else
        	{
        	  if(first) //add constants only in the first iteration
        	  {
        	    autoConst("%s#%s#%s".format(pred.name, args.indices.head, args.indices.last))._3  += ((
        	      if(quantifiedVars.contains(args.head.name)) "any" else args.head.name, 
        	      if(quantifiedVars.contains(args.last.name)) "any" else args.last.name))
        	  }
        	}
        	Set((args.head.name, args.last.name, "%s#%s#%s".format(pred.name, args.indices.head, args.indices.last)));
       	case _ => e.visit(findConstVarsQuantif, (x:List[Set[(String, String, String)]])=> x.reduce( _ ++ _))
      }
  }
  
  //assume all inference rules are of the form  Univ LHS conjunctions => RHS conjunctions
  private def findArrowsIR(e: FolExpression) : Set[(String, String, String)] =
  {
	  e match 
      {
      	case FolAllExpression(v, term) => quantifiedVars += v.name; findArrowsIR(term);
      	case FolExistsExpression(v, term) => quantifiedVars += v.name; findArrowsIR(term);      	
      	case FolIfExpression(lhs, rhs) =>  {
      		val lhsVars = findArrowsIR(lhs);
      		val rhsVars = findArrowsIR(rhs)
   			rhsVars.foreach(rhsVar => propagate(lhsVars, rhsVar));
      		/*
   			{
   				var lhsMatchedVars: scala.collection.mutable.Set[String] = scala.collection.mutable.Set();
   				lhsVars.foreach(lhsVar => {
   					if(lhsVar._3 != rhsVar._3) // do not add self-loops
   					{
   					  if (!(Set(lhsVar._1, lhsVar._2) & Set(rhsVar._1, rhsVar._2)).isEmpty) //there is variables overlap
   					    lhsMatchedVars += lhsVar._3
   					}
      			})
      			if (!lhsMatchedVars.isEmpty)
      				propagate (lhsMatchedVars.toSet, rhsVar._2);
      			/*  //No need for this case anymore because it can not happen 
      			else
      			{
      			  //RHS variable is not bounded with a LHS variable. 
      			  //In this case, do not generate an arrow, 
      			  //but add all constants in the type to the predicate.
      			  //This type of inference rules is wrong. I better remove it
      			  //It is removed
      			  val t = autoConst(rhsVar._2)._1;
      			  autoConst(rhsVar._2)._2 ++= allConstants(t);      			  
      			}*/
      		})*/
      		Set();
      	}
      	case FolAtom(pred, args @ _*) => 
      	  Set((args.head.name, args.last.name, "%s#%s#%s".format(pred.name, args.indices.head, args.indices.last)));
      	case _ => e.visit(findArrowsIR, (x:List[Set[(String, String, String)]])=> x.reduce( _ ++ _))
      }
  }
}
