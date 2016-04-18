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
import org.apache.commons.logging.LogFactory


object SetGoalPTP{
	  var negatedGoal: Boolean = false; //used in AutoTyping when generating negative evd
	}

class SetGoalPTP(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  private val LOG = LogFactory.getLog(classOf[SetGoalPTP])
  private val newVarNameSuffix = "_hPlus";
  
  /**
   * Return the proof, or None if the proof failed
   */
  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Seq[Double] = {

	SetGoalPTP.negatedGoal = false;
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
 	 extraHardEvidence = Set();
	 
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
    	var modifiedGoal = goal;
    	var entVarName = ""
    	if (Sts.qaRightAnswer != "") //this is QA not RTE
    	{
    		entVarName = Sts.qaEntities.get("@placeholder").get + "_q"
    		modifiedGoal = sortPreds(modifiedGoal, entVarName);
    	}
        val ent_h = FolApplicationExpression(FolVariableExpression(Variable("entailment_h")), FolVariableExpression(Variable(entVarName)));
		if(modifiedGoal.isInstanceOf[FolVariableExpression])
  		  	//handling a very special case of parsing error where the expression is just an empty box
			modifiedGoal = FolApplicationExpression(FolVariableExpression(Variable("dummyPred")), FolVariableExpression(Variable( "x" )));
		
        val expr_h = GoalExpression((universalifyGoalFormula(modifiedGoal -> ent_h )).asInstanceOf[FolExpression], Double.PositiveInfinity);
        extraExpressions = List(expr_h);
    }
    //---------------------RTE - Fix DCA (OLD WRONG code)--------------------------
    else if (Sts.opts.task == "rte" && Sts.opts.fixDCA == true && Sts.opts.softLogicTool == "mln")
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

    //---------------------RTE on SampleSearch--------------------------
    else if (Sts.opts.task == "rte" && (Sts.opts.softLogicTool == "ss" || Sts.opts.softLogicTool == "mln") )
    {
      //simple conjunction, one goal, no entailment predicate
      //quantifiers = Map();  //moved to introductionEntry
      //constantsCounter = 0;	//moved to introductionEntry
      isQuery = true;
      val goalExist = if(goal.isInstanceOf[FolVariableExpression])
    	  				(None, None)  //handling a very special case of parsing error
    	  						//where the expression is just an empty box
				      else 
				        introductionEntry(goal);

      var expr = goal;
      SetGoalPTP.negatedGoal = false;
      var countUniv = 0;
      quantifiers.foreach(q=>{
        if(q._2._1 == "A" && q._2._2 == false ||q._2._1 == "E" && q._2._2 == true)
          countUniv = countUniv + 1;
      })
      
      if(Sts.opts.softLogicTool == "ss")
      {
	      if (2*countUniv < quantifiers.size) //if Univs are less than Exists, negate
	      {
	        None
	    	expr = -goal;
	        SetGoalPTP.negatedGoal = true;
	      }
      }
      else
      {
    	if (Sts.opts.noHMinus)
    		expr = goal -> SetVarBindPTP.entPred_h;
    	else
    		expr = goal <-> SetVarBindPTP.entPred_h;
      }      
      extraExpressions = List(GoalExpression(expr.asInstanceOf[FolExpression], Double.PositiveInfinity));
      
      goalExist._1 match {
        case Some(g) => extraExpressions = HardWeightedExpression(g, Double.PositiveInfinity) :: extraExpressions;   //<<--- this is correct 
        case _ =>
      }
      
      goalExist._2 match {
        case Some(g) => extraExpressions = HardWeightedExpression(g, Sts.opts.wFixCWA /* * (if (Sts.opts.logOddsW) 1; else 6)*/ ) :: extraExpressions;   //<<--- this is correct 
        case _ =>
      }

      //----------------------
      assumptions //apply the same introduction procedure to the Text. 
        .foreach
        {
          case HardWeightedExpression(e, w) => {
		      //quantifiers = Map();	//moved to introductionEntry
		      //constantsCounter = 0;	//moved to introductionEntry
		      isQuery = false;
		      val textExist = if(e.isInstanceOf[FolVariableExpression])
	    	  					(None, None)  //handling a very special case of parsing error
	    	  						//where the expression is just an empty box
	    	  				else 
	    	  					introductionEntry(e);
		      assert(textExist._2.isEmpty);
		      textExist._1 match {
 	          	case Some(t) => extraExpressions = HardWeightedExpression(t, Double.PositiveInfinity) :: extraExpressions; true;	//<<--- this is correct 
		        case _ => false;
		      }
				textExist._2 match {
					 case Some(g) => throw new RuntimeException("No weighted evidence from T") //extraExpressions = HardWeightedExpression(g, Sts.opts.wFixCWA) :: extraExpressions;   //<<--- this is correct 
					 case _ =>
				}


          }
          case _ => false;
        }

    }
    //---------------------RTE - no Fix DCA--------------------------
/*    else if (Sts.opts.task == "rte" && Sts.opts.fixDCA == false)
    {
      //simple conjunction, one goal
      val expr = goal -> SetVarBindPTP.entPred_h;
      extraExpressions = List(GoalExpression(expr.asInstanceOf[FolExpression], Double.PositiveInfinity));
    }
    * 
*/
    
    //===================== ERROR =============================
    else if (Sts.opts.softLogicTool != "none")
      throw new RuntimeException("Not possible to reach this point")

	 //println (">>>>>>>>>Extra Evid" + extraHardEvidence.toList)
    val res = delegate.prove(newConstants, declarations, extraHardEvidence.toList ++ evidence,/*TODO*/ /*(This is wrong)-->*/ /*assumptions ++ extraExpressions  */  /*(This is correct)-->*/ extraExpressions ++ assumptions, null)
    //val res = Seq(0.0)
    if (SetGoalPTP.negatedGoal)
    {
      require(res.size == 1);
      if (res.head >= 0)
        Seq(1 - res.head)
      else
    	res
    }else res;
  }

  //****************************** SampleSearch functions *************************
  private var quantifiers: Map[String, (String, Boolean)] = null;
  private var constantsCounter = 0;
  private var isQuery = true;
  private var extraSoftEvidence:Set[FolExpression] = Set();
  private var extraHardEvidence:Set[FolExpression] = Set();
  //private var parent: FolExpression = null;
  
  //first expression is for universal quantifiers, second is for negated existentials
  private def introductionEntry(expr: FolExpression): (Option[FolExpression], Option[FolExpression]) =
  {
  	 extraSoftEvidence = Set();
    //extraHardEvidence = Set();
    quantifiers = Map();
    constantsCounter = 0;
    var first = (if (Sts.opts.lhsOnlyIntro )
    	lhsOnlyIntroduction(expr, false, false, List());
    else
    	introduction(expr, false, null)
    )
    
	 //println("####" + extraHardEvidence)
    var extraSoftEvidenceRule:Option[FolExpression]  = None
    extraSoftEvidence.foreach (x=> {
		if (extraSoftEvidenceRule == None)
			extraSoftEvidenceRule = Some(x)
		else
			extraSoftEvidenceRule = Some(FolAndExpression(extraSoftEvidenceRule.get, x));
    })
    if (extraSoftEvidence.size > 0 || (extraSoftEvidence == 1 && !extraSoftEvidence.head.isInstanceOf[FolEqualityExpression]))
    {
		var extraSoftEvidenceVariables = extraSoftEvidenceRule.get.getVariables;
		var univVars:Set[Variable]  = Set();
		extraSoftEvidenceVariables.foreach (v => 
		{
		  val vname = v.name.replace(newVarNameSuffix, "");
		  //require(quantifiers.contains(vname), vname + " not found in  " + quantifiers);
		  if (quantifiers.contains(vname)) //if the variable is not in quantifiers, then it is a constant, and should not be quantified
		  {
	          val q = quantifiers(vname);
	          if (/*(isQuery && Sts.opts.ratio) ||*/ !isQuery /*for univ in E, all generated preds are exist*/ || q._1 == "A" && q._2 == false || q._1 == "E" && q._2 == true)
	        	  univVars = univVars + v
	          else
	        	  extraSoftEvidenceRule = Some(FolAllExpression(v, extraSoftEvidenceRule.get))
		  }
        })
		univVars.foreach (v => 
		{
        	  extraSoftEvidenceRule = Some(FolExistsExpression(v, extraSoftEvidenceRule.get))
        })
    }
    else
    	extraSoftEvidenceRule = None
/*   
    if (!Sts.opts.withExistence && !isQuery)
    {
		first = None
		extraSoftEvidenceRule = None
	 }
    
	if (!Sts.opts.withFixUnivInQ && isQuery)
   	first = None

   if (!Sts.opts.withFixCWA && isQuery)
      extraSoftEvidenceRule = None

 */
    return (first, extraSoftEvidenceRule)
  }
  
  private def lhsOnlyIntroduction(expr: FolExpression, inLhs:Boolean, isNegated:Boolean, univs:List[String]): Option[FolExpression] =
  {
    expr match {
      case FolExistsExpression(variable, term) => {
        quantifiers = quantifiers 	++  Map( variable.name  ->  ("E", isNegated) );
        lhsOnlyIntroduction(term, inLhs, isNegated, univs)
      }
      case FolAllExpression(variable, term) => {
        quantifiers = quantifiers 	++  Map( variable.name  ->  ("A", isNegated) );
        lhsOnlyIntroduction(term, inLhs, isNegated, univs)
      }
      case FolNegatedExpression(term) => lhsOnlyIntroduction(term, false, !isNegated, univs);      
      case FolOrExpression(first, second) => lhsOnlyIntroduction(FolAndExpression(first, second), inLhs, isNegated, univs);
      case FolIfExpression(first, second) => 
      {
          val f = lhsOnlyIntroduction(first, true, !isNegated, univs);
 		  val s = lhsOnlyIntroduction(second, false, isNegated, univs);
 		  if (f.isEmpty && s.isEmpty)
 			  return None;
 		  else if (f.isEmpty)
 			  return s;
 		  else if (s.isEmpty)
 			  return f;
 		  else return  Some(FolAndExpression(f.get, s.get));
      }
      case FolAndExpression(first, second) =>
      {
         val f = lhsOnlyIntroduction(first, inLhs, isNegated, univs);
		  val s = lhsOnlyIntroduction(second, inLhs, isNegated, univs);
		  if (f.isEmpty && s.isEmpty)
			  return None;
		  else if (f.isEmpty)
			  return s;
		  else if (s.isEmpty)
			  return f;
		  else return  Some(FolAndExpression(f.get, s.get));
      }
      case FolAtom(pred, args @ _*) =>
      {
			val introStatus = isLhsOnlyIntroduction(pred.name, args, inLhs, univs, isNegated)
			var extraEvid:FolExpression = null
			if (introStatus != NO_EVD) 
				extraEvid = FolAtom.apply(pred, args.map(arg => lhsOnlyIntroduction(arg, univs) ) :_ *)
			introStatus match {
				case NO_EVD => None
				case HARD_EVD => extraHardEvidence = extraHardEvidence + extraEvid; Some(extraEvid);
				case SOFT_EVD => extraSoftEvidence = extraSoftEvidence + extraEvid; None;
			}
      }
      case FolVariableExpression(v) => Some(FolVariableExpression(lhsOnlyIntroduction(v, univs)));
      case FolEqualityExpression(first, second) => 
      {
         val introStatus = isLhsOnlyIntroduction("eq", Seq(first.asInstanceOf[FolVariableExpression].variable,
                    second.asInstanceOf[FolVariableExpression].variable), inLhs, univs, isNegated)
         var extraEvid:FolExpression = null
         if (introStatus != NO_EVD)
            extraEvid = FolEqualityExpression(lhsOnlyIntroduction(first, inLhs, isNegated, univs).get,
								                     lhsOnlyIntroduction(second, inLhs, isNegated, univs).get)
         introStatus match {
            case NO_EVD => None
            case HARD_EVD => extraHardEvidence = extraHardEvidence + extraEvid; Some(extraEvid)
            case SOFT_EVD => extraSoftEvidence = extraSoftEvidence + extraEvid; None;
         }
      }
      case FolIffExpression(first, second) => throw new RuntimeException(expr + " is not a valid expression")      
      case _ => throw new RuntimeException(expr + " is not a valid expression")
      }
  }
  private def lhsOnlyIntroduction(v: Variable, univs:List[String]): Variable =
  {
	var newVarName = v.name;
	if(quantifiers.contains(newVarName))
	{
		if(isQuery)
			newVarName = newVarName + newVarNameSuffix;
		addConst(newVarName);
	}
	return Variable(newVarName);
  }
  private val SOFT_EVD = 'S';
  private val HARD_EVD = 'H';
  private val NO_EVD = 'N';

  private def isLhsOnlyIntroduction (predName: String, args: Seq[Variable], inLhs:Boolean, univs:List[String], isNegated: Boolean): Char  = 
  {
        var univCount = 0;
        var notExistCount = 0;
        var constCount = 0
        args.foreach(arg=>{
          if (!quantifiers.contains(arg.name))//"Quntifiers: " + quantifiers + " do not contain " + arg.name);
          	constCount = constCount + 1  //this is a constant that was generated from the coreference resolution step. Do nothing with it.
          else
          {
	          val q = quantifiers(arg.name);
	          if (q._1 == "A" && q._2 == false || q._1 == "E" && q._2 == true)
	            univCount = univCount + 1;
	          if (q._1 == "E" && q._2 == true)
	        	  notExistCount = notExistCount + 1;
          }
        })
	var allUniv:Boolean = false;

/*	if (!isQuery && Sts.opts.ratio && isNegated)
		return SOFT_EVD
*/
	if(univCount == 0)
	{
		if (isNegated && isQuery)  //Existentially quantified negated predicate in query "e.g: tweety is not black"
			return if(Sts.opts.withFixCWA)  SOFT_EVD; else NO_EVD;
		return NO_EVD; //Nothing need to be done in case no Univ quantifiers
	}
	else if (univCount == args.length)
		allUniv = true;

//	println ("QE: " + isQuery + " isNegated: "+ isNegated + " allUniv: " + allUniv + " univCount: " + univCount)
//	if (!isNegated && !isQuery && allUniv)
//		println ("!isNegated && !isQuery && allUniv") 

	if(!isNegated)  //all evidence generated for unviersally quantified predicates is for negated predicates 
		return NO_EVD;
	
	if (inLhs /*hard evidence for stuff in the LHS (both in Text and Hypothesis) */
   || (allUniv && univCount == 1 && Sts.opts.evdIntroSingleVar /*&& (predName.contains("hungry") || predName.contains("man") || predName.contains("delicious") || predName.contains("food") ) ugly special case for sentences in the synthetic dataset, because we do not fully recognize all LHS and RHS of all quantifier*/ ))
   {
      if(Sts.opts.withExistence && !isQuery)
         return HARD_EVD;
      else if (Sts.opts.withFixUnivInQ && isQuery)
         return HARD_EVD;
      else if(Sts.opts.withFixCWA && isQuery) //if FixUnivInQ is disabled, then fall-back to FixCWA
         return SOFT_EVD;
      else 
         return NO_EVD;
   }

   if (isQuery) //soft evidence for all univ not in the LHS (only in Hypothesis)
      return if(Sts.opts.withFixCWA)  SOFT_EVD; else NO_EVD;
			
	return NO_EVD;

/*
	if (!isNegated)
		return false
	else if (isQuery) 
		return true  
	else if (allUniv && !predName.contains("agent") && !predName.contains("patient") && !predName.contains("eat") && univCount == 1) // in the Evidence. 
		return true   
	else 
		return false
*/
/*
	if(!isQuery  && inLhs && notExistCount == 0 && allUniv)  //Introduction for the exsitance case in Text
		return true;  //hard evidence
	else if (!isQuery  && isNegated && allUniv )  //Introduction for the exsitance case in Text
	{
		if(predName.contains("agent") || predName.contains("patient"))
			return false;  //hard evidence
		else return true;
	}
	else if(!isQuery)
		return false; //all other cases of Univ in Text should be ignored
	else if (isNegated)
		return true
	else  (!isNegated)
		return false
*/
/*	else if (allUniv)
	{
		if (isQuery && inLhs && notExistCount == 0)  /1LHS introduction in Q
			return true;  //hard evidence
		else if (isQuery && inLhs && notExistCount != 0)  //Impossible case
			throw new RuntimeException ("Unsupported case: in LHS, and some univ are notExist");
		else if (isQuery && !inLhs && notExistCount == 0)  //All birds are blue
		{
			System.out.println ("All univ are univ, but in RHS + " + isNegated)
			return false;  //all birds are blue. Generate a bird, but do not generate a blue
		}
		else if (isQuery && !inLhs && notExistCount != 0 && notExistCount < univCount)  //Not sure, probably do as notExistCount == univCount
		{
			System.out.println ("All univ, some Univ, some notExist + " + isNegated)
			//e.g: "all birds do not eat all food"
			assert (isNegated) //turns out predicates has to be negated for this to be necessary  
			return true;  //soft evidence
		} 
		else if (isQuery && !inLhs && notExistCount == univCount)
		{
			System.out.println ("All univ, all notExist + " + isNegated)
			assert (isNegated) //turns out predicates has to be negated for this to be necessary  
			return true;  //soft evidence
		}
		else
			throw new RuntimeException ("Unreachable point");
	}
	else
	{
		System.out.println ("Not all univ, some Univ, some Exist")
	    assert(args.length == 2);
	    assert(univCount == 1);
	    assert(notExistCount == 1 || notExistCount == 0);
	    if (notExistCount == 1) //one exist and one negated exist
	    {
	        assert (!inLhs) // can not reach here and be inLhs
       		System.out.println ("Not all univ, one exist, one notExist + " + isNegated)
       		if (isNegated)
       			return true;
       		else return false
	    }
	    else if (notExistCount == 0) //one exist and one univ
	    {
	        assert (!inLhs) //can not reach here and be inLhs
       		System.out.println ("Not all univ, one exist, one univ + " + isNegated)
	        return false; //nothing need to be done
	    }
	    else 
   			throw new RuntimeException ("Unreachable point");
	}*/
  }
  //---------------------------
  private def introduction(expr: FolExpression, isNegated:Boolean, parent:FolExpression): Option[FolExpression] =
  {
	expr match {
      case FolExistsExpression(variable, term) => {
        quantifiers = quantifiers 	++  Map( variable.name  ->  ("E", isNegated) );
        introduction(term, isNegated, expr) 
      }
      case FolAllExpression(variable, term) => {
        quantifiers = quantifiers 	++  Map( variable.name  ->  ("A", isNegated) );
        introduction(term, isNegated, expr)
      }
      case FolIfExpression(first, second) => introduction(FolAndExpression(first, second), isNegated, expr);
      case FolOrExpression(first, second) => introduction(FolAndExpression(first, second), isNegated, expr);
      case FolAndExpression(first, second) =>  {
         val f = introduction(first, isNegated, expr);
		  val s = introduction(second, isNegated, expr);
		  if (f.isEmpty && s.isEmpty)
			  return None;
		  else if (f.isEmpty)
			  return s;
		  else if (s.isEmpty)
			  return f;
		  else return  Some(FolAndExpression(f.get, s.get));
      }
      case FolAtom(pred, args @ _*) =>{
    	if(isIntroduction(args, isNegated, parent) )
    	{
    	   Some(FolAtom.apply(pred, args.map(arg => introduction(arg, isNegated) ) :_ *));
    	  /*if(parent.isInstanceOf[FolAndExpression])
    		  Some(FolAtom.apply(pred, args.map(arg => introduction(arg, isNegated) ) :_ *));
    	  else
    	  {
    		System.err.println(">>>>>>The unicorn case<<<<<<");
    	    None
    	  }*/
    	}
    	else 
    	  None
      }
	  case FolVariableExpression(v) => Some(FolVariableExpression(introduction(v, isNegated)));
	  case FolNegatedExpression(term) => introduction(term, !isNegated, expr);
      case FolEqualityExpression(first, second) => {
        	if(!isNegated && isIntroduction(Seq(first.asInstanceOf[FolVariableExpression].variable, 
        						   second.asInstanceOf[FolVariableExpression].variable), isNegated, parent))
        		Some(FolEqualityExpression(introduction(first, isNegated, expr).get, introduction(second, isNegated, expr).get));
        	else 
        	  None
      }
      case FolIffExpression(first, second) => throw new RuntimeException(expr + " is not a valid expression")      
	  case _ => throw new RuntimeException(expr + " is not a valid expression")
	}
  }
  private def introduction(v: Variable, isNegated:Boolean): Variable =
  {
		  require(quantifiers.contains(v.name));
          val q = quantifiers(v.name);
          //variable should be universally quantified
          require(q._1 == "A" && q._2 == false || q._1 == "E" && q._2 == true)   
          var newVarName = v.name;  
          if(isQuery)
            newVarName = newVarName + "_hPlus";

          if(q._1 == "E")
          {
            newVarName = newVarName + "_" + constantsCounter;
            constantsCounter = constantsCounter + 1;
          }
          addConst(newVarName);
          return Variable(newVarName);
  }
  private def isIntroduction (args: Seq[Variable], isNegated:Boolean, parent:FolExpression): Boolean = 
  {
        var univCount = 0;
        var notExistCount = 0;
        args.forall(arg=>{
          require(quantifiers.contains(arg.name));
          val q = quantifiers(arg.name);
          if (q._1 == "A" && q._2 == false || q._1 == "E" && q._2 == true)
            univCount = univCount + 1;
          if (q._1 == "E" && q._2 == true)
        	  notExistCount = notExistCount + 1;          
          true
        })
        if(univCount == args.length ) // all variables are universally quantified
        {
        	/*if(notExistCount == args.length )
        	{
        		parent match 
        		{
        			case FolAndExpression(first, second) => {
        				var count = 0;
        				first match 
        				{
        					case FolAtom(pred, args @ _*) => count = count + 1; 
        					case FolAndExpression(f,s) => count = count + 1;
        					case _ =>;
        				}	
        				second match 
        				{
        					case FolAtom(pred, args @ _*) => count = count + 1; 
        					case FolAndExpression(f,s) => count = count + 1;
        					case _ =>;        					
        				}
        				if(count != 2)
        					return false
        			}
        			case _=> 
        					System.out.println(">>>>>>The unicorn case<<<<<<");
        					return false;
        		}
        	}*/

		   if(notExistCount > 1/*any negated existntially quantitied relation*/ /*|| (!isQuery && notExistCount != 0 *//*any negated existentially quantitied predicate in the TEXT*//*)*/ )  
				return false;

			//TODO: try not to generate constants for relations that are NotExist. (Line above)
			//TODO: still some causes generate inconsistant MLNs. Also, not sure if the handling below is correct
        	if(parent.isInstanceOf[FolAndExpression])
        	{
        		if(notExistCount == 0)
        			System.out.println(">>>>>>INTRO<<<<<<" + args + ", " + parent);
        		return true;
        	}
        	else {
				System.out.println(">>>>>>The unicorn case<<<<<<");
				return false;
        	}
        }
        else
        {
        	if(univCount  != 0)
        	  LOG.trace("found %s variables, only %s of them is/are universlly quantified".format(args.length, univCount)) 
         return false
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
    	addConst(newVarName);
    	Variable(newVarName)
    }
    else 
    	v
  }
  
  private def addConst(varName: String) =
		newConstants += (varName.substring(0, 2) -> (newConstants.apply(varName.substring(0, 2)) + varName))

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
  
  //sort predicates so they form a connected graph
  private def sortPreds(input: FolExpression, answerVar:String): FolExpression =
  {
	//..
	val atoms = getAnds(input).map(x =>
	{
		x match
		{
			case FolAtom(pred, args @ _*) => (x, args.map(v => v.name))
			case _ => throw new RuntimeException("expression is not atom: " + x );
		}
	});
	
	val varQueue = new scala.collection.mutable.Queue[String];
	val visitedVars = new scala.collection.mutable.HashSet[String];
	varQueue.enqueue(answerVar);
	val equation = new scala.collection.mutable.ArrayBuffer[FolExpression];
	while (!varQueue.isEmpty)
	{
		val currentVar = varQueue.dequeue();
		
		// find all relations with one variable = currentVar and the other variable in visited
		var tmpAtoms = atoms.filter( a => a._2.length == 2  && a._2.contains(currentVar)  && !a._2.toSet.intersect(visitedVars).isEmpty );
		
		// Add these relations  to the equation
		equation ++= tmpAtoms.map(a => a._1);
		
		// find all predicate with one variable = currenrVat
		tmpAtoms = atoms.filter( a => a._2.length == 1  && a._2.contains(currentVar) );
		
		// add predicates sorted by pos to the equation
		//TODO

		// Add these predicates  to the equation
		equation ++= tmpAtoms.map(a => a._1);
				
		// find all relations with two variables, one of them = currentVar and the other is NOT in visited
		tmpAtoms = atoms.filter( a => a._2.length == 2  && a._2.contains(currentVar)  && a._2.toSet.intersect(visitedVars).isEmpty );
		
		// add currentVar to visited
		visitedVars += currentVar;

		
		// add all of the other variables to the queue
		tmpAtoms.flatMap(a => a._2).foreach(v =>
		{
			if (v != currentVar && !varQueue.contains(v))
			{
				//if (visitedVars.contains(v))
				//	null;
				//else
					varQueue.enqueue(v);
			}
		})
	}

	var break = false;
	var newGoal:FolExpression = null;
	for (atom <- equation)
	{
		if (newGoal == null)
			newGoal = atom;
		else
			newGoal = FolAndExpression(newGoal, atom);
	}
	
	var tmp = input;
	while (!break)
	{
		tmp match {
			case FolExistsExpression(variable, term) => {
			  newGoal = FolExistsExpression(variable, newGoal);
			  tmp = term;
			}
			case _ => break = true;
		}
	}
	
	newGoal
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
