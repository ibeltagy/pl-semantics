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
    goal: FolExpression): Seq[Double] = {

    var negatedGoal = false;
    var extraExpressions: List[WeightedExpression[FolExpression]] = List();
    newConstants = constants;//extra constants are added by skolemNew
	 flipQ = false;
	 univVars = List();
	 ands = List();
	 nonRelationsMap = List();
	 relationsMap = List();
	 notEqMap = List();
	 impMap = List();
	 entPred = null;
	 
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
        val ent_t = FolApplicationExpression(FolVariableExpression(Variable("entailment_t")), FolVariableExpression(Variable("")));
        val ent_h = FolApplicationExpression(FolVariableExpression(Variable("entailment_h")), FolVariableExpression(Variable("")));        
        val expr_t = GoalExpression((universalifyGoalFormula(first -> ent_t )).asInstanceOf[FolExpression], Double.PositiveInfinity);
        val expr_h = GoalExpression((universalifyGoalFormula(second -> ent_h )).asInstanceOf[FolExpression], Double.PositiveInfinity);
        val expr_h_t = GoalExpression(FolVariableExpression(Variable("entailment")).asInstanceOf[FolExpression], Double.NegativeInfinity);
        extraExpressions = List(expr_h, expr_t, expr_h_t);
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
    else if (Sts.opts.task == "rte" && Sts.opts.softLogicTool == "psl")
    {
      throw new RuntimeException("RTE is not supported on PSL");
    }
    //---------------------RTE on SampleSearch--------------------------
    else if (Sts.opts.task == "rte" && Sts.opts.softLogicTool == "ss")
    {
      //simple conjunction, one goal, no entailment predicate
      quantifiers = Map();
      val goalExist = ssGoal(goal);
      val expr = goal;
      negatedGoal = false;
      extraExpressions = List(GoalExpression(expr.asInstanceOf[FolExpression], Double.PositiveInfinity));
      goalExist match {
        case Some(g) => extraExpressions = extraExpressions ++List(HardWeightedExpression(g)); 
        case _ =>
      }

    }
    //---------------------RTE - no Fix DCA--------------------------
    else if (Sts.opts.task == "rte" && Sts.opts.fixDCA == false)
    {
      //simple conjunction, one goal
      val expr = goal -> SetVarBindPTP.entPred_h;
      extraExpressions = List(GoalExpression(expr.asInstanceOf[FolExpression], Double.PositiveInfinity));
    }
    //---------------------RTE - Fix DCA--------------------------
    else if (Sts.opts.task == "rte" && Sts.opts.fixDCA == true)
    {
      //H+, H-
      flipQ = false;
      univVars = List();
      var hPlusExp = skolemNew(goal -> SetVarBindPTP.entPred_h);
      univVars.foreach (v => hPlusExp = FolAllExpression (v, hPlusExp));   //apply univVars
      val hPlus = GoalExpression(hPlusExp, Double.PositiveInfinity);
      flipQ = true;
      val hMinus = GoalExpression(skolemNew(SetVarBindPTP.entPred_h -> goal, List(), true), Double.PositiveInfinity)
		if(Sts.opts.noHMinus)
	      extraExpressions = List(hPlus); // hMinus is disabled because of the computational overhead. 
		else 
			extraExpressions = List(hPlus, hMinus);
    } 

    //===================== ERROR =============================
    else
      throw new RuntimeException("Not possible to reach this point")

    val res = delegate.prove(newConstants, declarations, evidence, assumptions ++ extraExpressions, null)
    if (negatedGoal)
    {
      require(res.size == 1);
      if (res.head > 0)
        Seq(1 - res.head)
      else
    	res
    }else res;
  }

  //****************************** ssQuery functions *************************
  private var quantifiers: Map[String, (String, Boolean)] = null;
  
  private def ssGoal(expr: FolExpression, isNegated:Boolean = false): Option[FolExpression] =
  {
	expr match {
      case FolExistsExpression(variable, term) => {
        quantifiers = quantifiers 	++  Map( variable.name  ->  ("E", isNegated) );
        ssGoal(term, isNegated) 
      }
      case FolAllExpression(variable, term) => {
        quantifiers = quantifiers 	++  Map( variable.name  ->  ("A", isNegated) );
        ssGoal(term, isNegated)
      }
      case FolIfExpression(first, second) => ssGoal(first, isNegated);
      case FolAndExpression(first, second) => {
        val f = ssGoal(first, isNegated);
        val s = ssGoal(second, isNegated);
        if (f.isEmpty && s.isEmpty)
          return None;
        else if (f.isEmpty)
          return s;
        else if (s.isEmpty)
          return f;
        else return  Some(FolAndExpression(f.get, s.get));
      }

      case FolAtom(pred, args @ _*) =>{
        var univCount = 0;
        args.forall(arg=>{
          require(quantifiers.contains(arg.name));
          val q = quantifiers(arg.name);
          if (q._1 == "A" && q._2 == false || q._1 == "E" && q._2 == true)
            univCount = univCount + 1;
          true
        })
        if(univCount == 0 ) // all variables are existentially quantified
        	return None;
        require(univCount == args.length) // all variables are universally quantified
        Some(FolAtom.apply(pred, args.map(arg => skolemNew(arg, List(arg)) ) :_ *));  
      }
	  case FolVariableExpression(v) => Some(FolVariableExpression(skolemNew(v, List(v))));

	  case FolNegatedExpression(term) => None//throw new RuntimeException(expr + " is not a valid expression")
      case FolOrExpression(first, second) => throw new RuntimeException(expr + " is not a valid expression")
      case FolIffExpression(first, second) => throw new RuntimeException(expr + " is not a valid expression")
      case FolEqualityExpression(first, second) => throw new RuntimeException(expr + " is not a valid expression")
	  case _ => throw new RuntimeException(expr + " is not a valid expression")
	}
  }
  
  //****************************** fixDCA functions **************************  
  private var newConstants: Map[String, Set[String]] = null; 
  private var flipQ = false;
  private var univVars: List[Variable] = List();  
  private def skolemNew(expr: FolExpression, skolemVars: List[Variable] = List(), isNegated:Boolean = false): FolExpression = 
  {
	expr match {
      case FolExistsExpression(variable, term) => {
    	  if (isNegated)
    	  {
    	    if(!flipQ)
    	    {	//FolExistsExpression(variable, skolemNew(term, skolemVars, isNegated))
                univVars = univVars ++ List(variable);
		skolemNew(term, skolemVars, isNegated);
	    }
    	    else 
    	    	FolAllExpression(variable, skolemNew(term, skolemVars, isNegated))
    	  }
    	  else
    		  skolemNew(term, skolemVars++List(variable), isNegated)
      }
      case FolAllExpression(variable, term) => {
          if (!isNegated)
          {
        	  if(!flipQ)
        	  {	  //FolAllExpression(variable, skolemNew(term, skolemVars, isNegated))
	                  univVars = univVars ++ List(variable);
			  skolemNew(term, skolemVars, isNegated);
		  }
        	  else
        		  FolExistsExpression(variable, skolemNew(term, skolemVars, isNegated))
          }
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

  //****************************** PSL functions **************************
  private def universalifyGoalFormula(goalFormula: FolIfExpression) = {
    val FolIfExpression(goal, consequent) = goalFormula

    def isConjoinedAtoms(e: FolExpression): Boolean = {
      e match {
        case FolAtom(_, _*) => true
        case FolAndExpression(a, b) => isConjoinedAtoms(a) && isConjoinedAtoms(b)
        case _ => false
      }
    }

    def universalify(e: FolExpression): FolExpression = {
      e match {
        case FolExistsExpression(v, term) => FolAllExpression(v, universalify(term))
        case _ if isConjoinedAtoms(e) => e -> consequent
        case _ => e -> consequent // sys.error(e.toString)
      }
    }

    universalify(goal)
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
	        
	        notUsedNonRelations = notUsedNonRelations filterNot (args1++args2 contains);
	        
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
  //****************************** END Mini-clauses functions **************************
}
