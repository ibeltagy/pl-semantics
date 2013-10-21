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

class SetGoalPTP(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Option[Double] = {

    var extraExpressions: List[WeightedExpression[FolExpression]] = List();

  	//=====================Start STS=============================    
    if (Sts.opts.task == "sts")
    {
      //get text and hypothesis from the combined goal
      val (first, second) = goal match {
        case FolAndExpression(first, second) => (first, second)
        case _ => throw new RuntimeException("Invalid goal")
      }

      //---------------------STS on PSL--------------------------      
      if (Sts.opts.softLogicTool == "psl")
      {
        //simple conjunction, two goals
        val expr_t = GoalExpression((first -> SetVarBindPTP.entPred_t).asInstanceOf[FolExpression], Double.PositiveInfinity);
        val expr_h = GoalExpression((second -> SetVarBindPTP.entPred_h).asInstanceOf[FolExpression], Double.PositiveInfinity);
        //TODO: fix bug. Handle Existential quatifiers under negation         
        extraExpressions = List(expr_h, expr_t);
      }

      //---------------------STS on MLN--------------------------
      else //if(Sts.opts.softLogicTool == "mln") //(for both mln and none)
      {
        //mini clauses
    	extraExpressions = getMiniClausesWeightedExpressions(first, SetVarBindPTP.entPred_t) ++ 
    							getMiniClausesWeightedExpressions(second, SetVarBindPTP.entPred_h)    							
      }
    }
    //=====================Start RTE=============================
    //---------------------RTE - no Fix DCA--------------------------
    else if (Sts.opts.task == "rte" && Sts.opts.fixDCA == false)
    {
      //simple conjunction, one goal
      val expr = goal -> SetVarBindPTP.entPred_h;
      //TODO: fix bug. MLN does not handle Existential quantifier under negation right
      extraExpressions = List(GoalExpression(expr.asInstanceOf[FolExpression], Double.PositiveInfinity));
    }

    //---------------------RTE - Fix DCA--------------------------
    else if (Sts.opts.task == "rte" && Sts.opts.fixDCA == true)
    {
      //H+, H-
      //TODO: implement
    } 

    //===================== ERROR =============================
    else
      throw new RuntimeException("Not possible to reach this point")

    delegate.prove(constants, declarations, evidence, assumptions ++ extraExpressions, null)

  }

  //****************************** Mini-clauses functions ************************** 
  private def getMiniClausesWeightedExpressions(expr: FolExpression, entPred:FolExpression): List[WeightedExpression[FolExpression]] = {
    extractPartsOfExpression(expr);
    var miniClauses = getMiniClauses();
    val n = miniClauses.length;
    val w = (SetPriorPTP.entPrior + scala.math.log(Sts.opts.maxProb) - scala.math.log(1-Sts.opts.maxProb))/n;
    
    return miniClauses.map( clause =>
    {
    	var rule:FolExpression = clause -> entPred;
      	rule.getVariables.foreach(v => rule = FolAllExpression(v, rule));
      	GoalExpression(rule, w);
    })
  }
  private var ands :List[FolExpression] = List();//get a list of the anded predicates
  private var nonRelationsMap : List[(FolExpression, Set[Variable])] = List();
  private var relationsMap : List[(FolExpression, Set[Variable])] = List();//relation predicates are 2 valued and start with r_. 
      															//e.g: agent, patient, in, ...
  private var notEqMap : List[(FolExpression, Set[Variable])] = List();//expressions of the form !(x1=x2)
  private var impMap : List[(FolExpression, Set[Variable])] = List();//expressions of the form a->(b^c^...)
  private var entPred : FolExpression  = null;
      
  private def extractPartsOfExpression (expr: FolExpression) = 
  {
	  ands = getAnds(expr);
	  nonRelationsMap = List();
	  relationsMap = List();
	  notEqMap = List();
	  impMap = List();
	  for(expr <- ands )
	  {
	    val allVars = expr.getVariables; 
	    expr match
	    {
	      case FolApplicationExpression(fun, arg) =>{
		        fun match {
		          case FolApplicationExpression (fun2, arg2) =>
		            fun2 match {
		              case FolVariableExpression(v) =>{
		            	  if (v.name.startsWith("r_"))
		            	    relationsMap = (expr, allVars) :: relationsMap  ;
		            	  else
		            	    nonRelationsMap = (expr, allVars) :: nonRelationsMap;
		              }		                
		              case _ => nonRelationsMap = (expr, allVars) :: nonRelationsMap;
		            }
		          case FolVariableExpression(v) =>{
		        	  if (!v.name.startsWith("topic_"))
		        		  nonRelationsMap = (expr, allVars) :: nonRelationsMap;
		          }
		          case _=>  nonRelationsMap = (expr, allVars) :: nonRelationsMap;
		        }
	        }
	      case FolEqualityExpression(first, second)=>; //delete equality expressions because it is already handeled by renaming variables
	      case FolAllExpression(first, second)=>  impMap  = (expr, allVars) :: impMap;
	      case FolNegatedExpression(term)=> {
	        term match {
	          case FolEqualityExpression(first, second) => notEqMap  = (expr, allVars) :: notEqMap;
	          case _ => nonRelationsMap = (expr, allVars) :: nonRelationsMap; //this case will be changed later
	        }
	      };
	      case _ => nonRelationsMap = (expr, allVars) :: nonRelationsMap;
	    }
	  }
  }

  //count and print mini-clauses according to the chopLvl
  def getMiniClauses(): List[FolExpression] = 
  {
    var miniClauses:MutableList[FolExpression] = MutableList();
	//write relation predicates
    if (Sts.opts.chopLvl == "rp"){
        ////////////////////// man(x) ^ agent(x, y)
	      for(nonRelationExpr<- nonRelationsMap )
	      {
	    	  var printedAtLeastOnce = false;
		      for(relationExpr<- relationsMap )
		      {
		    	  if ((nonRelationExpr._2 & relationExpr._2).size != 0)
		    	  {
		    		  miniClauses += (nonRelationExpr._1 & relationExpr._1)
		    		  printedAtLeastOnce = true;
		    	  }
		      }
		      if (!printedAtLeastOnce)
		        miniClauses += (nonRelationExpr._1 )
	      }
	   ////////////////////// man(x) ^ agent(x, y)
      } else if (Sts.opts.chopLvl == "prp") {
             ////////////////////// man(x) ^ agent(x, y) ^ drive(y)
	    var notUsedNonRelations = nonRelationsMap;
	    
	    relationsMap.foreach(rel => {
	    	val args1 = nonRelationsMap.filter(p => p._2.contains(rel._2.head))
	        val args2 = nonRelationsMap.filter(p => p._2.contains(rel._2.last))
	        
	        notUsedNonRelations = notUsedNonRelations -- (args1++args2);
	        
	        args1.foreach(arg1 =>
	            args2.foreach(arg2 =>
	            	miniClauses += (arg1._1 & rel._1 & arg2._1)
	            ) 
	        )
	        if (args1.size == 0 || args2.size == 0){
	        	(args1 ++ args2).foreach(arg =>
	        		miniClauses += (arg._1 & rel._1) 
	            ) 
	        }
	    })
	    notUsedNonRelations.foreach(pred => miniClauses += pred._1 )
             ////////////////////// man(x) ^ agent(x, y) ^ drive(y)            
      }		    
  
      //write notEqual predicates
      for(notEqExpr <- notEqMap )
      {
          var oneLine = notEqExpr._1;
          var foundOne = false;
	      for(nonRelationExpr <- nonRelationsMap )
	      {
	    	  if ((notEqExpr._2 & nonRelationExpr._2).size != 0)
	    	  {
	    		  oneLine = oneLine & nonRelationExpr._1;
	    		  foundOne = true;
	    	  }
	      }
	      if (foundOne)
	    	 miniClauses += oneLine
      }
      
      //write imp expressions 
      for(impExpr <- impMap )
    	  miniClauses += impExpr._1;
      
      return miniClauses.toList;
  }

  //get a list of the anded predicates
  private def getAnds(input: FolExpression): List[FolExpression] =
    input match {
      case FolExistsExpression(variable, term) => getAnds(term) // don't add outermost 'exist'
      case _ => _getAnds(input)
    }

  private def _getAnds(input: FolExpression): List[FolExpression] =
    input match {
      case FolAndExpression(first, second) => _getAnds(first) ++  _getAnds(second) 
      case _ => List(input);
    }
  

  
  
}
