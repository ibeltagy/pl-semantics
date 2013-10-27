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
import utcompling.scalalogic.discourse.candc.boxer.expression._
import org.apache.commons.logging.LogFactory
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter

class PositiveEqEliminatingProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  private val LOG = LogFactory.getLog(classOf[PositiveEqEliminatingProbabilisticTheoremProver])
  private var extraEvd:List[BoxerExpression] = List();

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {

    //Remove it only from the Text because that help generating the evidence.
    //No need to do it for the hypothesis 
    val newAssumptions:List[WeightedExpression[BoxerExpression]] = assumptions.map
    {
          case HardWeightedExpression(e) => {
	        	inNot = false;
	        	equalities = List();
	        	evdBeforeEqRemove = List()
				var newExpr = skolemConstAsEvd(e, true, true)
				equalities = groupEqvClasses(equalities);
				LOG.trace(equalities)
				extraEvd = extraEvd ++ evdBeforeEqRemove.map(applyEq);
				newExpr = applyEq(newExpr); 
	        	HardWeightedExpression(newExpr)
	        }
	        case a @ _ => a
    }
    
    delegate.prove(
      constants,
      declarations,
      evidence ++ extraEvd,
      newAssumptions,
      goal)
  }
  
  private var evdBeforeEqRemove:List[BoxerExpression] = List();
  private var equalities:List[Set[String]] = List();
  private var inNot = false;
  private var negatedConstCount = 0;
  
  private def skolemConstAsEvd(e: BoxerExpression, outer: Boolean, remove: Boolean): BoxerExpression = 
  {
    if(!outer) //no need to continue if not in the outer most existentially quantified variables 
    			//nor the LHS of a the outer most Implication 
      return e;

    e match {
      case BoxerAlfa(variable, first, second) => BoxerAlfa(variable, skolemConstAsEvd(first, outer, remove),
    		  														skolemConstAsEvd(second, outer, remove));
      case BoxerMerge(pred, first, second) => BoxerMerge(pred, skolemConstAsEvd(first, outer, remove), 
    		  													skolemConstAsEvd(second, outer, remove));
      case BoxerApp(function, argument) => BoxerApp(skolemConstAsEvd(function, outer, remove), 
    		  										skolemConstAsEvd(argument, outer, remove));      
      case BoxerProp(discId, indices, variable, drs) => BoxerProp(discId, indices, variable, skolemConstAsEvd(drs, outer, remove));
      
      case BoxerImp(discId, indices, first, second) => BoxerImp(discId, indices, skolemConstAsEvd(first, true, false), 
    		  																	 skolemConstAsEvd(second, false, false));
      
      case BoxerNot(discId, indices, drs) => {
        inNot = true;
        val exp = BoxerNot(discId, indices, skolemConstAsEvd(drs, outer, false))
        inNot = false;
        exp;
      }
      case BoxerDrs(refs, conds) => 
      {
        var conditions:List[BoxerExpression] = List();
        if (inNot)
          	conditions = conds.flatMap(c=> //change variables names, but do no remove them
	        {
	        	negatedConstCount = negatedConstCount + 2;	          
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
	        	  case BoxerOr(discId, indices, first, second) => List(BoxerOr(discId, indices, skolemConstAsEvd(first, outer, remove), skolemConstAsEvd(second, outer, remove)))
	        	  case _ => List(skolemConstAsEvd(c, outer, remove)) 
	            } 
	        })
        else 
	        conditions = conds.flatMap(c=> 
	        {
	        	c match
	            {
	        	  case BoxerEq(discId, indices, first, second) => equalities = equalities ++ List(Set(first.name, second.name)); None;
	        	  case BoxerPred(discId, indices, variable, name, pos, sense) => evdBeforeEqRemove = evdBeforeEqRemove :+ c; None;
	        	  case BoxerNamed(discId, indices, variable, name, typ, sense) => evdBeforeEqRemove = evdBeforeEqRemove :+ c; None;
	        	  case BoxerRel(discId, indices, event, variable, name, sense) => evdBeforeEqRemove = evdBeforeEqRemove :+ c; None;
	        	  case BoxerCard(discId, indices, variable, num, typ) => evdBeforeEqRemove = evdBeforeEqRemove :+ c; None;
	        	  case BoxerOr(discId, indices, first, second) => evdBeforeEqRemove = evdBeforeEqRemove :+ c; None;
	        	  case _ => List(skolemConstAsEvd(c, outer, remove)) 
	            } 
	        })
	    
	    if(inNot)
	    	BoxerDrs(refs, conditions)
        else if(remove)
        	BoxerDrs(List(), conditions)
        else
        	BoxerDrs(refs, conds)
      }
      case _ => throw new RuntimeException(e + " is not possible")
    }
  }
  
  private def findRemoveEq(e: BoxerExpression): BoxerExpression = 
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
  
  private def applyEq(e: BoxerExpression): BoxerExpression = {
    e match {
      case BoxerVariable(v) => {
	    equalities.foreach(eq => {
	    	if (eq.contains(v))
	  		  return BoxerVariable(eq.head);
	  	})
		BoxerVariable(v)
      }
      case _ => e.visitConstruct(applyEq)
    }
  }
}
