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
import utcompling.mlnsemantics.util.TimeoutUtil


class AutoTypingPTP(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  private var allConstants: Map[String, Set[String]] = null;
  private var autoConst : scala.collection.mutable.Map[String, (Seq[String], scala.collection.mutable.Set[(String, Seq[String])])] = null; //predName#varIndx -> (type, H

  //private var arrows : scala.collection.mutable.Set[(Set[String], String)] = null; //predName#varIndx#varIndx -> predName#varIndx#varIndx 
  private var extraEvid: List[FolExpression] = null;
  private var quantifiedVars: scala.collection.mutable.Set[String] = null;
  private var isHardRule = true;
  private var isRuleR = false; //Rule added by SetGoalPTP to solve DCA problems in Q
  //private var repeat = true;
  private var first = true;
  private var goalExp:FolExpression = null; //process Goal at the end
  private var goalW:Double = 0;

  private var textExp:FolExpression = null; //process Text second time at the end
  private var textW:Double = 0;

  private var goalPredsPolarity:scala.collection.mutable.Map[String, String] = null //predName->polarity
  private val POSITIVE = "P";
  private val NEGATIVE = "N";
  private val BOTH = "B";
  private val INTRODUCTION = "I"
 
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
    goalExp = null; //process Goal at the end
    goalW = 0;

    textExp = null; //process Text a second time at end
    textW = 0;

	 goalPredsPolarity = null;
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
			
			first = true;
			var repeat = 0;
			while (repeat <= 1)
			{
				repeat = repeat + 1;
				//1)collect constants, and propagate them according to the Text
							  
				assumptions.foreach 
				{
					case HardWeightedExpression(e, w) => 
					{
						processExp(e, w);
                  textExp = e //last HardWeightedExpression is the Text. This is ugly but works (hopefully)
                  textW = w
					}
					case GoalExpression(e, w) => 
					{
						goalExp = e
						goalW = w
					}
					case SoftWeightedExpression(e, w) =>
					{
						quantifiedVars.clear();
						isHardRule = false;
						findConstVarsQuantif(e);
						findArrowsIR (e)
					}
					case _ => ;
				}
				//processExp(goalExp, goalW) //process Goal rule at the end
            //processExp(textExp, textW) //process Text rule again at the end
            first = false;
			}//Repeat
			genNegativeEvd(declarations);
		}
	
		//findApply
		val finish = TimeoutUtil.runWithTimeoutJava(10000) { findApply ;  true }
		if(finish < 0)
			return Seq(-5.0)
    }
    
    delegate.prove(
      allConstants,
      declarations,
      (evidence.toSet ++ extraEvid.toSet).toList,  //toSet to remove duplicate evidences
      assumptions,
      goal)
    
  }
  private def processExp (e:FolExpression, w:Double) =
  {
	quantifiedVars.clear();
	isHardRule = true;
	isRuleR = (w != Double.PositiveInfinity && w == Sts.opts.wFixCWA ); // Rule added by SetGoalPTP to solve DCA problems in Q
	val vars = findConstVarsQuantif(e);
	var allPropagatedConstToSkolemIndexed:Map[String, List[List[String]]] = Map();//dataset to store previously propagated skolem
																						//constants to avoid propagating it twice 
	var lastAutoConstSize:Int = autoConst.map(_._2._2.size).reduce(_+_);
	findArrowsIR(e); //Run it at least once
	/*
	vars.foreach(rhsVar => {
		vars.foreach(lhsVar => {
			if (!lhsVar._1.startsWith("r_"))
				propagate(Set(lhsVar), rhsVar)
		})
	})
	 */
	while (lastAutoConstSize != autoConst.map(_._2._2.size).reduce(_+_))
	{
		lastAutoConstSize = autoConst.map(_._2._2.size).reduce(_+_)
		//println (lastAutoConstSize)
		
	//val skolemFunctionsCount = vars.count(_._1.startsWith("skolem"));
	//repeat the propagation cycle: propagate, generate on Skolem, propagate, ....
	//for (i <- 0 to skolemFunctionsCount)
	//{
			//println("iteration " + i);
			//Propagate all from LHS of IMP to RHS (note that this also propagates to SKOLEM)
			//findArrowsIR(e)  //<<------------this "was" the right thing to do 
			/*vars.foreach(rhsVar => 
			{
			  //if (rhsVar._1.startsWith("skolem"))
				  vars.foreach(lhsVar => propagate(Set(lhsVar), rhsVar))
			})*/

			 //All propagations to skolem are done. 
			//Now, call genPermutes from HardAssumptionAsEvidenceProbabilisticTheoremProver
			vars.foreach(rhsVar => 
			{
			  if (rhsVar._1.startsWith("skolem"))
			  {
				val inVarCount = HardAssumptionAsEvidenceProbabilisticTheoremProver.getInputVarCount(rhsVar._1)
				val totalVarCount = rhsVar._2.size
				val propagatedConstToSkolem = autoConst(rhsVar._1);
				val propagatedConstToSkolemCleaned = propagatedConstToSkolem._2.unzip._2
				autoConst(rhsVar._1)._2.retain(!_._2.contains("any"));
				var maxLen = 0;//longest list of constants per variable
				val propagatedConstToSkolemIndexed = (0 until inVarCount).map(indx => {
					val allConstAtIndex = propagatedConstToSkolemCleaned.map(tuple => tuple(indx)).toSet.toList;
					val previouslyPropagated = allPropagatedConstToSkolemIndexed.getOrElse(rhsVar._1, List());
					val allConstAtIndexCleaned = allConstAtIndex.filter(c => c != "any" && 
														(previouslyPropagated.isEmpty || !previouslyPropagated(indx).contains(c) ));
				  //println(indx + "--" + allConstAtIndexCleaned)
					maxLen = math.max(maxLen, allConstAtIndexCleaned.size)
					allConstAtIndexCleaned
				}).toList
				/*
				if (allPropagatedConstToSkolemIndexed.getOrElse(rhsVar._1, List()).isEmpty)
					//initialize it
					allPropagatedConstToSkolemIndexed = allPropagatedConstToSkolemIndexed + (rhsVar._1 -> propagatedConstToSkolemIndexed.toList)
				else
					//add to the previous
					allPropagatedConstToSkolemIndexed = allPropagatedConstToSkolemIndexed + (rhsVar._1 -> ((0 until inVarCount).map(indx => { 
						allPropagatedConstToSkolemIndexed.get(rhsVar._1).get(indx) ++ propagatedConstToSkolemIndexed(indx)
					})).toList)
					* 
					*/
				val outVar:List[String] = rhsVar._2.slice(inVarCount, totalVarCount).toList;
				val evidAndConst = HardAssumptionAsEvidenceProbabilisticTheoremProver.genPermutes (
										maxLen, inVarCount, rhsVar._1, propagatedConstToSkolemIndexed, outVar, autoConst(rhsVar._1)._2.map(_._2.take(inVarCount).toList).toSet);
				extraEvid = extraEvid ++ evidAndConst._1;
				evidAndConst._2.foreach(c =>addConst(c)) //update allConstants
				//add to autoConst
				autoConst(rhsVar._1)._2  ++= evidAndConst._1.map ( evid => evid match {
				  case FolAtom(pred, args @ _*) =>  ("CONST", args.map(_.name))
				})
			  }
			})
			//generate arrows from vars
			 findArrowsIR(e)

			//I would love to remove this code and replace it with findArrowsIR (Done above)
			/*vars.foreach(rhsVar => 
			{
			  //Do not propagate TO skolem predicates. DO all other propagations
			  //if (!rhsVar._1.startsWith("skolem")) 
				  vars.foreach(lhsVar => if (lhsVar._1.startsWith("skolem")) propagate(Set(lhsVar), rhsVar))
			})
			*/
		}
		//findArrowsIR(e)
  }
  
  private def collectGoalPredsPolarity(expr: FolExpression, univs: Set[String], isNegated:Boolean, lhs:Boolean):Any = 
  {
	expr match {
      case FolExistsExpression(variable, term) => if (isNegated) collectGoalPredsPolarity(term, univs + variable.name, isNegated, lhs);
																  else collectGoalPredsPolarity(term, univs, isNegated, lhs);
      case FolAllExpression(variable, term) => if(!isNegated) collectGoalPredsPolarity(term, univs + variable.name, isNegated, lhs);
                                                  else collectGoalPredsPolarity(term, univs, isNegated, lhs);
      case FolNegatedExpression(term) => collectGoalPredsPolarity(term, univs, !isNegated, false /*lhs*/)
      case FolAndExpression(first, second) => collectGoalPredsPolarity(first, univs, isNegated, lhs);
      										collectGoalPredsPolarity(second, univs, isNegated, lhs);
      case FolOrExpression(first, second) => collectGoalPredsPolarity(first, univs, isNegated, lhs);
      										collectGoalPredsPolarity(second, univs, isNegated, lhs);
      case FolIfExpression(first, second) => collectGoalPredsPolarity(first, univs, !isNegated, true/*lhs*/);  //no negation here. This is not a typo
      										collectGoalPredsPolarity(second, univs, isNegated, false/*lhs*/);
      case FolIffExpression(first, second) => throw new RuntimeException("FolIffExpression is not a valid expression")
      case FolEqualityExpression(first, second) => collectGoalPredsPolarity(first, univs, isNegated, lhs);
      										collectGoalPredsPolarity(second, univs, isNegated, lhs);	
      case FolAtom(pred, args @ _*) =>{
      	  val NOTHING = "Nothing"
      	  val univIntroduction = lhs || (args.size == 1 && univs.contains(args.head.name) && Sts.opts.evdIntroSingleVar);
      	  val polarity:String = if  (isNegated && univIntroduction )
										POSITIVE //INTRODUCTION
									else if (isNegated)
      	  							NEGATIVE 
      	  						else POSITIVE;
    	  val previous = goalPredsPolarity.getOrElse(pred.name, NOTHING)
    	  if (previous  == NOTHING)
    	  	goalPredsPolarity += (pred.name -> polarity)
    	  else if (previous != polarity)
    	  	goalPredsPolarity += (pred.name -> BOTH)
    	  
      } 
	  case FolVariableExpression(v) => 
	  case _ => throw new RuntimeException(expr + " is not a valid expression")
	}
  }

  private def toNegateOrNot(exp:FolExpression) : FolExpression=
  {
	/*val polarity  = exp match 
	{
		case FolAtom(pred, args @ _*) => goalPredsPolarity.getOrElse(pred.name, POSITIVE)
	}
	assert(polarity != BOTH)
	
  	if((GivenNotTextProbabilisticTheoremProver.negativeEvd && polarity == POSITIVE) 
  	|| (!GivenNotTextProbabilisticTheoremProver.negativeEvd && polarity == NEGATIVE)
	|| polarity == INTRODUCTION)
		return FolNegatedExpression(exp);
	else return exp
	* 
	*/
  	return FolNegatedExpression(exp);
  }
  	
  private def genNegativeEvd(declarations:Map[FolExpression, Seq[String]]) = 
  {    
	//collect goal predicates polarity
  	goalPredsPolarity = scala.collection.mutable.Map()
  	collectGoalPredsPolarity(goalExp, Set(), SetGoalPTP.negatedGoal, false)
  	//println (autoConst);
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
            	      if(!possibleConst.contains(c) && !possibleConst.contains("all"))
            	      {
            	    	  var negEvd: FolExpression = FolVariableExpression(Variable(pred.name));
            	    	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c )));
            	    	  negEvd = toNegateOrNot(negEvd);
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
            		      if(    !possibleConst.contains((c1, c2)) 
            		          && !( possibleConst.contains(c1, "any") && possibleConst.contains("any", c2))
            		          && !possibleConst.contains(c1, "all") 
            		          && !possibleConst.contains("all", c2)
            		          && !possibleConst.contains("all", "all")
            		        )
	            	      {
			            	  var negEvd: FolExpression = FolVariableExpression(Variable(pred.name));
			              	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c1 )));
			            	  negEvd = FolApplicationExpression(negEvd, FolVariableExpression(Variable( c2 )));
			            	  negEvd = toNegateOrNot(negEvd)
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
	  var skolemInVarsSeq :Seq[String]= rhs._2;  //Not the cleanest code ever, but who cares :P
	  var skolemOutVarsSeq :Seq[String]= lhs.head._2;  //Not the cleanest code ever, but who cares :P
	  //if (lhs.head._1.startsWith("skolem_1") && rhs._1.startsWith("europe"))
	  //	println("hi");
	  if (rhs._1.contains( "skolem"))//[OLD]do not propagate to "skolem" predicates because they are already close-world
	  {							//Propagate to "skolem" predicates because I am not generating all constants on it anymore
		  skolemInVarsSeq = rhs._2.slice(0, HardAssumptionAsEvidenceProbabilisticTheoremProver.getInputVarCount(rhs._1))
	  }
	  
	  val skolemPredsCountInLhs = lhs.filter(_._1.startsWith("skolem")).size 
	  if (skolemPredsCountInLhs > 0)
	  {
	    assert(skolemPredsCountInLhs == 1);
	    //assert(lhs.size == 1);
		//skolemOutVarsSeq = lhs.head._2.slice(HardAssumptionAsEvidenceProbabilisticTheoremProver.getInputVarCount(lhs.head._1), lhs.head._2.size)
	    val skolemPred = lhs.filter(_._1.startsWith("skolem")).head
	    skolemOutVarsSeq = skolemPred._2.slice(HardAssumptionAsEvidenceProbabilisticTheoremProver.getInputVarCount(skolemPred._1), skolemPred._2.size)
	  }
	  //assert(rhs._2.size < 3)  //this is wrong once we propagate to skolem predicates
	  
	  if((quantifiedVars & rhs._2.toSet).isEmpty) //intersection is empty, so all variables are not quantified
		  										//In case of skolem in LHS, this does not matter
	     return //if both rhs variables are Constants
	  
	  var allExtraConst:Set[(String, Seq[String])] = null; 
		  
	  val lhsConstSetsPropagated:List[Set[(String, Seq[String])]]  = lhs.toList.sortBy( l => -l._2.length).flatMap(lhsEntry=>{   //sorting to make predicates of multiple arguments come first. 
		  														//this is important for the intersection
		  try 
		  {
			  //This is wrong. Here is an example where: 
			  //agent(x,y) => agent(x, C)
			  //I want to propaget the "x"
			  //I still do not know its effect on the runtime, but I am guessing it is the same 
			  //because my recent code does not propagate individual variables, it propagates all variables 
		      //of a certain relation together
		      //if(lhsEntry._1 == rhs._1) // do not add self-loops
			  //  throw AllDone
		    
			  if (( lhsEntry._2.toSet & rhs._2.toSet & skolemInVarsSeq.toSet/*Initialized to rhs._2*/ ).isEmpty) //there is no variables overlap
			    throw AllDone
			  if (lhsEntry._1.startsWith("skolem") && (skolemOutVarsSeq.toSet & rhs._2.toSet).isEmpty) 
			    throw AllDone //there is no variables overlap between the OUT variables of the skolem predicate, and the RHS 
			    
			  var lhsConstSet = autoConst(lhsEntry._1)._2;
			  lhsEntry._2.indices.foreach(lhsVarIdx=>{
			    val lhsVar = lhsEntry._2(lhsVarIdx);
			    if(!quantifiedVars.contains(lhsVar)) //Constant not variable
				  lhsConstSet = lhsConstSet.map(c=>(c._1, c._2.updated(lhsVarIdx, lhsVar)));
			  })

			  val lhsConstSetPropagated:Set[(String, Seq[String])] = lhsConstSet.flatMap(lhsConst=>
			  {
    			  //TODO: if lhsConst._1 does not match the conditions, return None
			      //if(isHardRule && lhsConst._1 == rhs._1)
			      if(isHardRule && lhsConst._1 == "TEXT" && !rhs._1.contains( "skolem") ) //do not propagate if in TEXT and source of the constant is   
			    	  								//a former propagation step in the TEXT, unless you are propagating to SKOLEM
			      {
			        //println("CONST propagation canceled (TEXT)")
			        None
			      }
/*			      else if(!isHardRule && lhsConst._1 == "IR") //do not propagate is in IR and the const is resulting from 
			    	  										//a former application of IR
			      {
			        //println("CONST propagation canceled (IR)")
			        None
			      }
*/
			      else if (lhsConst._2.contains("all"))
			      {
			        //Do not propagated constants with "all" 
			        None
			      }
			      else
			      {
				      val lhsConstPropagated = rhs._2.map(rhsVar=>{ //loop over all vars of rhs even for skolem pred because I need
				    	  										//the extra "any" to be output in place of the OUT variables.
				        if(lhsEntry._2.contains(rhsVar))
				        {
				    	  val idx = lhsEntry._2.indexOf(rhsVar)
				    	  val const = lhsConst._2(idx);
				    	  if (const == "any")
				    	  	"X"
				    	  else
				    	  	const
				        }
				        else "any";
				      })//end rhsConst
				      //Some((if(isHardRule) lhsEntry._1 else lhsConst._1, lhsConstPropagated));
				      Some((if(isHardRule) "TEXT" /*lhsEntry._1*/ else "IR", lhsConstPropagated));
			      }
			  }).toSet//end lhsConst
			  //println (lhsEntry._1 + " " + lhsConstSetPropagated);
		  
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
		     Some(lhsConstSetPropagated)
		  } catch {case AllDone => None}
	  })
     if (lhs.size > 0) //always 
	  {
				var found = true;
				//println (lhs + " -> " + rhs + " ==>" + lhsConstSetsPropagated);
				val combined:List[List[String]] /*list of possible constants for each variable on the RHS*/= rhs._2.indices.map(rhsVarIndex=>
				{
					//variable not quantified, then it is a constant. 
					if (!quantifiedVars.contains(rhs._2(rhsVarIndex)))
					{
						List((rhs._2(rhsVarIndex))) //do not propagate, just return the constant as it is.
					}
					else
					{
						//for each column in the table lhsConstSetsPropagated 
						var foundConstInVar: Set[String] = null;
						lhsConstSetsPropagated.foreach ( lhsPredConst => {
							val constInPredInVar = lhsPredConst.unzip._2.filter(!_.contains("X")).map(_.apply(rhsVarIndex)).toSet
							//println ("constInPredInVar at index " +rhsVarIndex + " => "+ constInPredInVar);
							if (foundConstInVar == null)
								foundConstInVar = constInPredInVar
							else
								foundConstInVar = (if (foundConstInVar.contains("any"))  constInPredInVar
														else if (constInPredInVar.contains("any")) foundConstInVar
														else foundConstInVar & constInPredInVar)
							//println ("current foundConstInVar" + foundConstInVar)
						})
						if (foundConstInVar == null || foundConstInVar.isEmpty)
						{
							//found = false
							List[String]();
						}
						else
							foundConstInVar.toList
					}
				 }).toList
				//if (found)
				//	allExtraConst = Set( ( "TEXT", combined));
			//Generate all possible combinations using "combined"
			val maxLen = combined.map(_.size).max
	      	var combinations:Set[List[String]] = Set(); 
			  permut(List.range (0, maxLen), combined.size).foreach(c /*one possible combination*/ => { c.permutations/*permute the combination. what ??*/.foreach( p  /*one permutation*/=> {
			  var oneCombination: List[String] = List();
			  object AllDone extends Exception { }
			  try
			  {
			  	 for(i <- 0 to p.length-1)
		    	 {
		    	    val idx = p(i)
		    	    val constListForVariableI = combined(i);
		    	    if (constListForVariableI.size <= idx)
		    	      throw AllDone;
		    	    oneCombination = oneCombination :+ constListForVariableI(idx);
		    	 }
			  	 combinations = combinations + oneCombination;
			  }catch{
			      case AllDone =>//do nothing
			  }
		    }) /*END P*/ })
		 
		   //println(combinations)
		   allExtraConst = combinations.map(("TEXT", _))
				 
		}
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
  //generate permutations
  private def permut[A](as: List[A], k: Int): List[List[A]] = 
	(List.fill(k)(as)).flatten.combinations(k).toList

  private def findConstVarsQuantif(e: FolExpression): Set[(String, Seq[String])] =
  {
      e match 
      {
        case FolExistsExpression(v, term) => quantifiedVars += v.name; findConstVarsQuantif(term);  
        case FolAllExpression(v, term) => quantifiedVars += v.name; findConstVarsQuantif(term);
        case FolEqualityExpression(first, second) => Set()
        case FolAtom(pred, args @ _*) =>
        	if(quantifiedVars.contains(args.head.name) && quantifiedVars.contains(args.last.name) && !isRuleR)
        	{
        	  None; // do nothing
        	}
        	else
        	{
        	  if(first) //add constants only in the first iteration
        	  {
				 //repeat = true;   //TODO: this should be uncommented but after reducing constatns from SetGoal, and fix AutoTyping
				   val label = if(isRuleR) "POS"; else "CONST";
        	      autoConst(pred.name)._2  +=  ((label, args.map(arg=>{
        	    	  if(quantifiedVars.contains(arg.name)) "any" else arg.name
        	      })))
        	    
        	      //Special case for R generated by SetGoalPTP to solve DCA prbolems in Q
        	      if(isRuleR && /*args.length > 1 &&*/ (quantifiedVars.contains(args.head.name) || quantifiedVars.contains(args.last.name)))
        	      {
        	    	 //Using the flag "all" means its values it unknown for all constants. 
        	         //It also prevents these values from being propagated elsewhere. 
        	         //Actually, we need values to be propagated TO this predicate, not from it.
        	         //Not allowing these values to be propagated reduce has a nice effect of reducing the domain size. 
        	         autoConst(pred.name)._2  +=  ((label, args.map(arg=>{
        	    		  if(quantifiedVars.contains(arg.name)) "all" else arg.name
        	    	  })))        	        
        	      }
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
		case FolIfExpression(lhs, rhs) => {
			val lhsVars = findArrowsIR(lhs);
			val rhsVars = findArrowsIR(rhs)
			rhsVars.foreach(rhsVar =>{
				propagate(lhsVars, rhsVar)
			});
			lhsVars ++ rhsVars
		}
      case FolIffExpression(lhs, rhs) => {
         val lhsVars = findArrowsIR(lhs);
         val rhsVars = findArrowsIR(rhs)
         rhsVars.foreach(rhsVar =>{
            propagate(lhsVars, rhsVar)
         });
         lhsVars ++ rhsVars
      }
		case FolAtom(pred, args @ _*) => 
		  Set((pred.name, args.map(_.name)));
		case _ => e.visit(findArrowsIR, (x:List[Set[(String, Seq[String])]])=> x.reduce( _ ++ _))
	}
  }
  
  private def addConst(varName: String) =
		  allConstants += (varName.substring(0, 2) -> (allConstants.apply(varName.substring(0, 2)) + varName))
}

