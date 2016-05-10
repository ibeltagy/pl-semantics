package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import utcompling.mlnsemantics.run.Sts
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.inference.support.GoalExpression
import scala.collection.mutable.MutableList

class PositiveEqEliminatingProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  private val LOG = LogFactory.getLog(classOf[PositiveEqEliminatingProbabilisticTheoremProver])
  private val removeFolExp:FolExpression = FolVariableExpression(Variable("remove"));

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[FolExpression, Seq[String]],
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Seq[Double] = {

    newConstants = constants;//extra constants are added by skolemConstAsEvd
    equalities = List();
    val newGoal = apply(goal)
    //Rename variable of @placeholder based on equalities found on the goal
    Sts.qaEntities = Sts.qaEntities + ("@placeholder" -> applyEq(Sts.qaEntities("@placeholder"))) 
    //Remove it only from the Text because that help generating the evidence.
    //No need to do it for the hypothesis 
    val newAssumptions:List[WeightedExpression[FolExpression]] = assumptions.map
    {
		case HardWeightedExpression(e, w) => 
		{
			val newE = HardWeightedExpression(apply(e), w)
			//Rename variable of all entities except @placeholder based on equalities found on the goal
			val prevQaEntities = Sts.qaEntities
			Sts.qaEntities = Sts.qaEntities.empty
			prevQaEntities.foreach(p => {
				if (p._1 == "@placeholder")
					Sts.qaEntities = Sts.qaEntities + (p._1 -> p._2)
				else
					Sts.qaEntities = Sts.qaEntities + (p._1 -> applyEq(p._2))
			})
			newE //This is text
		}
		case GoalExpression(e, w) => throw new RuntimeException("Not reachable") //GoalExpression(apply(e), w) 
		case a @ _ => a
    }
    
    delegate.prove(
      newConstants,
      declarations,
      evidence,
      newAssumptions,
      newGoal)
  }
  private def apply (exp:FolExpression):FolExpression = 
  {
  	if (exp == null)
  		return exp;
  	equalities = List();
	var newExpr = findRemoveEq(exp, Set(), false);
	equalities = groupEqvClasses(equalities);
	q = List();
	newExpr = applyEq(newExpr);
	newConstants = newConstants.map(constant=>{
		(constant._1, constant._2.map(applyEq(_)).toSet )
	}).toMap
	LOG.trace(equalities + " removed in " + newExpr)
	return newExpr
  }
  private var equalities:List[Set[String]] = List();
  private var q:List[String] = List();  //quantifiers
  private var newConstants: Map[String, Set[String]] = null;
  
  def addConst(varName: String) =
		newConstants += (varName.substring(0, 2) -> (newConstants.apply(varName.substring(0, 2)) + varName))

/*		
  private def skolemConstAsEvd(e: BoxerExpression, outer: Boolean, remove: Boolean, isNegated: Boolean): BoxerExpression = 
  {
    if(!outer) //no need to continue if not in the outer most existentially quantified variables 
    			//nor the LHS of an outer most Implication 
      return e;

    e match {
      case BoxerAlfa(variable, first, second) => BoxerAlfa(variable, skolemConstAsEvd(first, outer, remove, isNegated),
    		  														skolemConstAsEvd(second, outer, remove, isNegated));
      case BoxerMerge(pred, first, second) => BoxerMerge(pred, skolemConstAsEvd(first, outer, remove, isNegated), 
    		  													skolemConstAsEvd(second, outer, remove, isNegated));
      case BoxerApp(function, argument) => BoxerApp(skolemConstAsEvd(function, outer, remove, isNegated), 
    		  										skolemConstAsEvd(argument, outer, remove, isNegated));      
      case BoxerProp(discId, indices, variable, drs) => BoxerProp(discId, indices, variable, skolemConstAsEvd(drs, outer, remove, isNegated));
      
      case BoxerImp(discId, indices, first, second) => BoxerImp(discId, indices, skolemConstAsEvd(first, true, false, isNegated), 
    		  																	 skolemConstAsEvd(second, false, false, isNegated));      
      case BoxerNot(discId, indices, drs) =>  BoxerNot(discId, indices, skolemConstAsEvd(drs, outer, remove, !isNegated))
      
      //TODO: I am not sure this is right
      case BoxerOr(discId, indices, first, second) => BoxerOr(discId, indices, skolemConstAsEvd(first, true, false, isNegated), 
    		  																	 skolemConstAsEvd(second, false, false, isNegated));
      case BoxerDrs(refs, conds) => 
      {
        var conditions:List[BoxerExpression] = List();
        if (isNegated)
          	conditions = conds.flatMap(c=> //change variables names, but do no remove them
	        {
	        	/*negatedConstCount = negatedConstCount + 2;	          
	        	c match
	            {
	        	  case BoxerEq(discId, indices, first, second) => None
	        	  case BoxerPred(discId, indices, variable, name, pos, sense) => evdBeforeEqRemove = evdBeforeEqRemove :+ 
	        	  					BoxerPred(discId, indices, BoxerVariable(variable.name + "_not"+negatedConstCount), name, pos, sense); List(c);
	        	  case BoxerNamed(discId, indices, variable, name, typ, sense) => evdBeforeEqRemove = evdBeforeEqRemove :+ 
	        			  			BoxerNamed(discId, indices, BoxerVariable(variable.name + "_not"+negatedConstCount), name, typ, sense); List(c);
	        	  case BoxerRel(discId, indices, event, variable, name, sense) => evdBeforeEqRemove = evdBeforeEqRemove :+ 
	        			  			BoxerRel(discId, indices, BoxerVariable(event.name + "_not"+negatedConstCount), 
	        			  									BoxerVariable(variable.name + "_not"+(negatedConstCount+1)), name, sense); List(c);
	        	  case BoxerCard(discId, indices, variable, num, typ) => evdBeforeEqRemove = evdBeforeEqRemove :+
	        			  			BoxerCard(discId, indices, BoxerVariable(variable.name + "_not"+negatedConstCount), num, typ); List(c);
	        	  					//TODO: are you sure this is correct ???
	        	  case BoxerOr(discId, indices, first, second) => List(BoxerOr(discId, indices, skolemConstAsEvd(first, outer, remove, isNegated), skolemConstAsEvd(second, outer, remove, isNegated)))
	        	  case _ => List(skolemConstAsEvd(c, outer, remove, isNegated)) 
	            }*/
	          	c match
	            {
	        	  case BoxerEq(discId, indices, first, second) =>  None;
	        	  case BoxerPred(discId, indices, variable, name, pos, sense) => 
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ BoxerNot(discId, indices, c);
	        	    		addConst(variable.name)
	        	    		None;
	        	  case BoxerNamed(discId, indices, variable, name, typ, sense) => 
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ BoxerNot(discId, indices, c);
	        	    		addConst(variable.name)
	        	    		None;
	        	  case BoxerRel(discId, indices, event, variable, name, sense) => 
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ BoxerNot(discId, indices, c);
	        	    		addConst(variable.name)
	        	    		addConst(event.name)
	        	    		None;
	        	  case BoxerCard(discId, indices, variable, num, typ) => 
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ BoxerNot(discId, indices, c);
	        	    		addConst(variable.name)
	        	    		None;
	        	  case BoxerTimex(discId, indices, variable, timeExp) =>
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ BoxerNot(discId, indices, c);
	        	    		addConst(variable.name)
	        	    		None;
	        	  //TODO: Are you sure OR is correct ? 
	        	  //I think yes, it is right
	        	  case BoxerOr(discId, indices, first, second) => skolemConstAsEvd(first, outer, remove, isNegated);
	        	  												skolemConstAsEvd(second, outer, remove, isNegated);
	        	  												None;
	        	  case _ => List(skolemConstAsEvd(c, outer, remove, isNegated)) 
	            }
	        })
        else 
	        conditions = conds.flatMap(c=> 
	        {
	        	c match
	            {
	        	  case BoxerEq(discId, indices, first, second) => 
	        	    		equalities = equalities ++ List(Set(first.name, second.name)); 
	        	    		addConst(first.name); 
	        	    		addConst(second.name); 
	        	    		None;
	        	  case BoxerPred(discId, indices, variable, name, pos, sense) => 
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ c;
	        	    		addConst(variable.name)
	        	    		None;
	        	  case BoxerNamed(discId, indices, variable, name, typ, sense) => 
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ c;
	        	    		addConst(variable.name)
	        	    		None;
	        	  case BoxerRel(discId, indices, event, variable, name, sense) => 
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ c;
	        	    		addConst(variable.name)
	        	    		addConst(event.name)
	        	    		None;
	        	  case BoxerCard(discId, indices, variable, num, typ) =>
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ c;
	        	    		addConst(variable.name)
	        	    		None;
	        	  case BoxerTimex(discId, indices, variable, timeExp) =>
	        	    		evdBeforeEqRemove = evdBeforeEqRemove :+ c;
	        	    		addConst(variable.name)
	        	    		None;
	        	  //TODO: I am not sure the way OR is handled is correct 
	        	  //Probably it is not.
	        	  //case BoxerOr(discId, indices, first, second) => evdBeforeEqRemove = evdBeforeEqRemove :+ c; None;
	        	  case _ => List(skolemConstAsEvd(c, outer, remove, isNegated)) 
	            } 
	        })
	    
	    /*if(inNot)
	    	BoxerDrs(refs, conditions)
        else*/ if(remove)
        	BoxerDrs(List(), conditions)
        else
        	BoxerDrs(refs, conds)
      }
      case _ => throw new RuntimeException(e + " is not possible")
    }
  }
  */
  private def getBoolean2(e:FolExpression) : Char = 
  {
    if(e == removeFolExp)
    	'r';
    else 
    	'o';
  }
    
  private def findRemoveEq(e: FolExpression, quantifiers: Set[String], isNegated: Boolean): FolExpression =
  {
      e match 
      {
      	case FolExistsExpression(v, term) => 
            val mTerm = findRemoveEq(term, quantifiers + (v.name), isNegated)
      		getBoolean2(mTerm)  match 
      		{
      		  case 'r' => removeFolExp
      		  case 'o' => FolExistsExpression(v, mTerm);
      		} 
        case FolAllExpression(v, term) =>
        	val mTerm = findRemoveEq(term, quantifiers + (v.name), isNegated)
      		getBoolean2(mTerm)  match 
      		{
      		  case 'r' => removeFolExp
      		  case 'o' => FolAllExpression(v, mTerm);
      		}
        case FolNegatedExpression(term) => 
            val mTerm = findRemoveEq(term, quantifiers, !isNegated) 
      		getBoolean2(mTerm)  match 
      		{
      		  case 'r' => removeFolExp
      		  case 'o' => FolNegatedExpression(mTerm); 
      		}            
        case FolAndExpression(first, second) =>
          	// for large structures, the recursive code breaks with StackOverFlow. 
          	//The code here is changed to iterative to sovle this problem 
            val conjuncts:scala.collection.mutable.Stack[FolExpression] = scala.collection.mutable.Stack();
          	var current = e
          	while (current.isInstanceOf[FolAndExpression])
          	{
          	  current match 
          	  {
          	    case FolAndExpression(left, right) =>
          	      assert (! right.isInstanceOf[FolAndExpression])
          	      val mRight = findRemoveEq (right, quantifiers, isNegated)
          	      //println (mRight)
		      	  getBoolean2(mRight)  match
		      	  {
		      	  	case 'r' => 
		      	  	case 'o' => conjuncts.push(mRight)
		      	  }
          	      current = left
          	  }
          	}
          	val mCurrent = findRemoveEq (current, quantifiers, isNegated)
          	//println(mCurrent)
          	getBoolean2(mCurrent)  match
          	{
      	  		case 'r' => 
      	  		case 'o' => conjuncts.push(mCurrent)
          	}
          	//println("conjuncts.length = " +  conjuncts.length)
          	if (conjuncts.length == 0)
          	  removeFolExp;
          	else if (conjuncts.length == 1)
          	  conjuncts.pop();
          	else 
          	{
          		var mE = conjuncts.pop()
          		while (!conjuncts.isEmpty)
          		  mE = FolAndExpression(mE, conjuncts.pop());
          		mE
          	}

          	/*
            val mFirst = findRemoveEq(first, quantifiers, isNegated) 
      		val mSecond = findRemoveEq(second, quantifiers, isNegated)
      		(getBoolean2(mFirst),getBoolean2(mSecond)) match 
      		{
      		  case ('r', 'r') => removeFolExp
      		  case ('r', 'o') => mSecond
      		  case ('o', 'r') => mFirst
      		  case ('o', 'o') => FolAndExpression(mFirst, mSecond);
      		}*/
      	case FolOrExpression(first, second) =>
      	    val mFirst = findRemoveEq(first, quantifiers, isNegated) 
      		val mSecond = findRemoveEq(second, quantifiers, isNegated)
      		(getBoolean2(mFirst),getBoolean2(mSecond)) match 
      		{
      		  case ('r', 'r') => removeFolExp
      		  case ('r', 'o') => mSecond
      		  case ('o', 'r') => mFirst
      		  case ('o', 'o') => FolOrExpression(mFirst, mSecond);
      		}
      	case FolIfExpression(first, second) =>
      	    val mFirst = findRemoveEq(first, quantifiers, !isNegated) 
      		val mSecond = findRemoveEq(second, quantifiers, isNegated)
      		(getBoolean2(mFirst),getBoolean2(mSecond)) match 
      		{
      		  case ('r', 'r') => removeFolExp
      		  case ('r', 'o') => mSecond
      		  case ('o', 'r') => FolNegatedExpression(mFirst)
      		  case ('o', 'o') => FolIfExpression(mFirst, mSecond);      		    
      		}      		
        case FolIffExpression(first, second) => throw new RuntimeException("not reachable")
        case FolEqualityExpression(first, second) => {
          val vFirst = first.asInstanceOf[FolVariableExpression].variable.name;
          val vSecond = second.asInstanceOf[FolVariableExpression].variable.name;
          if(quantifiers.contains(vFirst) && quantifiers.contains(vSecond))
          {
        	  //if(!isNegated)
        	equalities = equalities ++ List(Set(vFirst, vSecond));
        	removeFolExp
          }
		  	 else
			 	e
        }
          
        case FolAtom(pred, args @ _*) => e 
        case FolVariableExpression(v) => e
        case _ => throw new RuntimeException("not reachable" + e)
      }
  }
    
  
  /*
  private def findRemoveEq(e: FolExpression, isNegated: Boolean, quantifiers: Set[String]): FolExpression =
  {
    e match {
    	case FolExistsExpression(v, term) => FolExistsExpression(v, findRemoveEq(term, isNegated, quantifiers  +  v.name ))
    	case FolAllExpression(v, term) => FolAllExpression(v, findRemoveEq(term, isNegated, quantifiers  +  v.name ))
    	case FolNegatedExpression(term) => FolNegatedExpression(findRemoveEq(term, !isNegated, quantifiers))
    	case FolIfExpression(first, second) => FolIfExpression(findRemoveEq(first, !isNegated, quantifiers), findRemoveEq(second, !isNegated, quantifiers))
    	case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(rename(_)) :_ * )	
    	case FolVariableExpression(v) => FolVariableExpression(rename(v)) 
    	case _=>e.visitStructured(rename, e.construct)
    }
  }
    */
  /*
  private def findRemoveEq(e:FolExpression, isNegated: Boolean) : FolExpression = 
  {
    e match {
      case BoxerAlfa(variable, first, second) => BoxerAlfa(variable, findRemoveEq(first, isNegated),
    		  														findRemoveEq(second, isNegated));
      case BoxerMerge(pred, first, second) => BoxerMerge(pred, findRemoveEq(first, isNegated), 
    		  													findRemoveEq(second, isNegated));
      case BoxerApp(function, argument) => BoxerApp(findRemoveEq(function, isNegated), 
    		  										findRemoveEq(argument, isNegated));      
      case BoxerProp(discId, indices, variable, drs) => BoxerProp(discId, indices, variable, findRemoveEq(drs, isNegated));
      
      case BoxerImp(discId, indices, first, second) => {
         if(isNegated)
        	 BoxerImp(discId, indices, findRemoveEq(first, !isNegated), second);
         else 
           e;
      }      
      case BoxerNot(discId, indices, drs) =>  BoxerNot(discId, indices, findRemoveEq(drs, !isNegated))
      
      case BoxerOr(discId, indices, first, second) => e
      
      case BoxerDrs(refs, conds) => 
      {
        var conditions:List[BoxerExpression] = List();
        conditions = conds.flatMap(c=> 
        {
        	c match
            {
        	  case BoxerEq(discId, indices, first, second) =>
        	    	if(!isNegated)
        	    		equalities = equalities ++ List(Set(first.name, second.name));  
        	    	None;
        	  case BoxerPred(discId, indices, variable, name, pos, sense) => Some(c) 
        	  case BoxerNamed(discId, indices, variable, name, typ, sense) => Some(c)
        	  case BoxerRel(discId, indices, event, variable, name, sense) => Some(c)
        	  case BoxerCard(discId, indices, variable, num, typ) => Some(c)
        	  case BoxerTimex(discId, indices, variable, timeExp) => Some(c)
        	  case _ => List(findRemoveEq(c, isNegated)) 
            } 
        })
       	BoxerDrs(refs, conditions)
      }
      case _ => throw new RuntimeException(e + " is not possible")
    } 
  }
  
  */
  /*private def findRemoveEq(e: BoxerExpression): BoxerExpression = 
  {
    e match {

      case BoxerNot(discId, indices, drs) => {
        inNot = true;
        BoxerNot(discId, indices, findRemoveEq(drs))
      }
      case BoxerDrs(refs, conds) => {
        val currentInNot = inNot;  //use a local copy 
        inNot = false; //for the recursive call, always say not in NOT
        val conditions = conds.flatMap(c=> 
        {
        	if (c.isInstanceOf[BoxerEq])
            {
        		if(!currentInNot) //if not negated //TODO: it is not clear yet for me what equalities to remove and what not to. 
        		{
        		  val eq = c.asInstanceOf[BoxerEq]
        		  equalities = equalities ++List(Set(eq.first.name, eq.second.name)) //called only for non-negated equalities;
        		}        		
        		
        		List() //either negated or not, remove it. 
            }
           	else 
           		List(findRemoveEq(c)) 
        })
        BoxerDrs(refs, conditions)
      }
      case BoxerEq(discId, indices, first, second) => throw new RuntimeException("This point should not be reached");
      case _ => e.visitConstruct(findRemoveEq)
    }
  }
*/
  object AllDone extends Exception { }
  
  private def groupEqvClasses(eq: List[Set[String]]): List[Set[String]] =
  {
    //var equalities = eq.map(x => (x.head -> x)).toMap; //map is wrong x1=x2, x1=x3
    var groupedEq = eq;
    var changed = true;
    while (changed) {
      changed = false
      try{
          groupedEq.foreach(outerEq =>
          {
              groupedEq.foreach(innerEq =>
                {
                  if (outerEq != innerEq) //not the same entry
                  {
                    if ((outerEq & innerEq).size != 0) { //if there is an intersection 
                      groupedEq  = (outerEq ++ innerEq) :: groupedEq ; //group them in one set and add this set to the list
                      groupedEq = groupedEq.filter(e=> (e != outerEq && e != innerEq)); //remove the two small sets
                      changed = true;
                      throw AllDone;  //simulating "break". This is important because changing groupedEq messes up the outer two loops. 
                    }
                  }
                })
            })
        }catch {case AllDone =>}
    }
    return groupedEq;
  }
  
  private def applyEq(v: String): String =
  {
	equalities.foreach(eq => {
    	if (eq.contains(v))
  		  return eq.head;
  	})
	v	
  }
  
  private def applyEq(v: Variable): Variable =
  {
	Variable(applyEq(v.name))
  }
  

  private def applyEq(e: FolExpression): FolExpression = 
  {
    e match {
    	case FolExistsExpression(v, term) => 
			val changedVar = applyEq(v);
			//println ("changedVar: " + changedVar.name + " from: " + v.name + " q: " + q)
			if (q.contains(changedVar.name))
				applyEq(term)
			else
			{
				q = q :+ (changedVar.name);
				FolExistsExpression(changedVar, applyEq(term))	
			}

    	case FolAllExpression(v, term) => 
         val changedVar = applyEq(v);
         //println ("changedVar: " + changedVar.name + " from: " + v.name + " q: " + q)
         if (q.contains(changedVar.name))
            applyEq(term)
         else
         {
            q = q :+ (changedVar.name);
            FolAllExpression(changedVar, applyEq(term))
         }
    	case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(applyEq(_)) :_ * )	
    	case FolVariableExpression(v) => FolVariableExpression(applyEq(v)) 
        case FolAndExpression(first, second) =>
        {
          	// for large structures, the recursive code breaks with StackOverFlow. 
          	//The code here is changed to iterative to sovle this problem 
            val conjuncts:scala.collection.mutable.Stack[FolExpression] = scala.collection.mutable.Stack();
          	var current = e
          	while (current.isInstanceOf[FolAndExpression])
          	{
          	  current match 
          	  {
          	    case FolAndExpression(left, right) =>
          	      assert (! right.isInstanceOf[FolAndExpression])
          	      conjuncts.push( applyEq (right) )
          	      current = left
          	  }
          	}
          	conjuncts.push(applyEq(current))
          	LOG.trace("conjuncts.length = " +  conjuncts.length)
          	if (conjuncts.length == 0)
          	  removeFolExp;
          	else if (conjuncts.length == 1)
          	  conjuncts.pop();
          	else 
          	{
          		var mE = conjuncts.pop()
          		while (!conjuncts.isEmpty)
          		  mE = FolAndExpression(mE, conjuncts.pop());
          		mE
          	}
    	}
    	case _=>e.visitStructured(applyEq, e.construct)
    }
  }
  
}
