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


  
  /*private var oldConstants: Map[String, Set[String]] = null;
  private var newDeclarations: Map[FolExpression, Seq[String]] = null;

  private var skolemFunctionsCounter:Int = 0;
  private var skolemConstCounter:Int = 0; 
  */
  private var allConstants: Map[String, Set[String]] = null;
  private var autoConst : scala.collection.mutable.Map[String, (String, scala.collection.mutable.Set[String])] = null; //predName#varIndx -> (type, HX1, HX2 ....)
  private var arrows : scala.collection.mutable.Set[(String, String)] = null; //predName#varIndx -> predName#varIndx 
  private var extraEvid: List[FolExpression] = null;
  private var quantifiedVars: scala.collection.mutable.Set[String] = null;
  
  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Seq[Double] = {
    

/*    oldConstants = constants;
    newDeclarations = declarations;

  	skolemFunctionsCounter = 0;
  	skolemConstCounter = 0; 
*/
    allConstants = constants;
    extraEvid = List();
  	quantifiedVars = scala.collection.mutable.Set();
    autoConst = scala.collection.mutable.Map(); //predName#varIndx -> HX1, HX2 ....
    arrows =  scala.collection.mutable.Set(); //an x -> y means all constants of x should be propagated to y
    
    if(Sts.opts.negativeEvd)
    {
	    
	    declarations.foreach(d => {
	      d match {
	          case (FolAtom(pred, args @ _*), s) => { 
	            args.indices.foreach(i=>
	              {
	            	//allConstants(args(idx).name.substring(0, 2));
	                autoConst += ("%s#%s".format(pred.name, i)->((s(i), scala.collection.mutable.Set())));
	              })
	          }
	          case _ => throw new RuntimeException("Non-atomic declaration");
	      }
	    });
	        
	    assumptions.foreach{
	          case HardWeightedExpression(e) => findConsts(e);
	          case SoftWeightedExpression(e, w) => findArrows (e)
	          case _ => ;
	        }
	    
	    applyArrows();
	    
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
      d match {
          case (FolAtom(pred, args @ _*), s) => {
            if(!pred.name.startsWith("skolem"))
            {
	            val negEvdConsts = args.indices.map(i=>
	            {
	                val t = autoConst("%s#%s".format(pred.name, i));
	                allConstants(t._1) -- t._2; //all
	            })
	            if(negEvdConsts.length > 2 || negEvdConsts.length < 1 )
	              throw new RuntimeException("unsupported number of arguments of predicate " + pred.name);
	            if(negEvdConsts.length == 1)
	            {
	              negEvdConsts(0).foreach(c=>{
	            	  var negEvd: FolExpression = FolVariableExpression(Variable(pred.name));
	              	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c )));
	              	  negEvd = FolNegatedExpression(negEvd);
	              	  extraEvid = negEvd :: extraEvid;
	              })
	            }else if(negEvdConsts.length == 2)
	            {
	              negEvdConsts(0).foreach(c0=>{
	                val allC = allConstants( autoConst("%s#%s".format(pred.name, 1))._1 ) 
	                allC.foreach(c1=>{
	            	  var negEvd: FolExpression = FolVariableExpression(Variable(pred.name));
	              	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c0 )));
	            	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c1 )));
	              	  negEvd = FolNegatedExpression(negEvd);
	              	  extraEvid = negEvd :: extraEvid;
	                })
	              })
	              val allC = allConstants( autoConst("%s#%s".format(pred.name, 0))._1 )
                  allC.foreach(c0=>{
	                negEvdConsts(1).foreach(c1=>{
	            	  var negEvd: FolExpression = FolVariableExpression(Variable(pred.name));
	              	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c0 )));
	            	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c1 )));
	              	  negEvd = FolNegatedExpression(negEvd);
	              	  extraEvid = negEvd :: extraEvid;
	                })
	              })
	              
	            }
            }
          }
          case _ => throw new RuntimeException("Non-atomic declaration");
      }
    });
  }
  
  private def applyArrows() = 
  {
    var repeat = true;
    while(repeat)
    {
    	repeat = false;
    	arrows.foreach(arrow=>{
	      val rhsSetSize = autoConst(arrow._2)._2.size;
	      autoConst(arrow._2)._2 ++= autoConst(arrow._1)._2;
	      if (rhsSetSize != autoConst(arrow._2)._2.size)
	        repeat = true
		})
    }
  }

  private def findConsts(e: FolExpression): Any =
  {
      e match 
      {
      	case FolExistsExpression(v, term) => quantifiedVars += v.name; findConsts(term);  
        case FolAllExpression(v, term) => quantifiedVars += v.name; findConsts(term);
        case FolAtom(pred, args @ _*) => args.indices.foreach(idx =>{
        	if(quantifiedVars.contains(args(idx).name))
        	{
        	  //insert ALL in autoConst at pred#idx
        	  autoConst("%s#%s".format(pred.name, idx))._2 ++= allConstants(args(idx).name.substring(0, 2));
        	}
        	else
        	{
        	  //insert args(idx).name in autoConst at pred#idx
        	  autoConst("%s#%s".format(pred.name, idx))._2 += args(idx).name;
        	}
        })
        case _ => e.visit(findConsts, (x:List[Any])=> 0)
      }
  }
  
  //assume all inference rules are of the form  Univ LHS conjunctions => RHS conjunctions
  private def findArrows(e: FolExpression) : List[(String, String)] =
  {
	  e match 
      {
      	case FolAllExpression(v, term) => findArrows(term);
      	case FolIfExpression(lhs, rhs) =>  {
      		val lhsVars = findArrows(lhs);
      		val rhsVars = findArrows(rhs)
   			rhsVars.foreach(rhsVar => {
   				var matchFound:Boolean = false; 
   				lhsVars.foreach(lhsVar => {
      				if(lhsVar._1 == rhsVar._1)
      				{
      				  matchFound = true;
      				  if(lhsVar._2 != rhsVar._2) // do not add self-loops
      					  arrows += ((lhsVar._2, rhsVar._2))
      				}
      			})
      			if (!matchFound)
      			{
      			  //RHS variable is not bounded with a LHS variable. 
      			  //In this case, do not generate an arrow, 
      			  //but add all constants in the type to the predicate.
      			  //This type of inference rules is wrong. I better remove it.
      			  val t = autoConst(rhsVar._2)._1;
      			  autoConst(rhsVar._2)._2 ++= allConstants(t);      			  
      			}
      		})
      		List();
      	}
      	case FolAtom(pred, args @ _*) => args.indices.map(idx =>{
        	  (args(idx).name , "%s#%s".format(pred.name, idx));
        }).toList;
      	case _ => e.visit(findArrows, (x:List[List[(String, String)]])=> x.reduce( _ ++ _))
      }
  }
}
