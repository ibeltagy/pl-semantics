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
				var newExpr = findRemoveEq(e)
				equalities = groupEqvClasses(equalities);
				LOG.trace(equalities)
				newExpr = applyEq(newExpr);
	        	HardWeightedExpression(newExpr)
	        }
	        case a @ _ => a
    }
    
    delegate.prove(
      constants,
      declarations,
      evidence,
      newAssumptions,
      goal)
  }
  
  private var equalities:List[Set[String]] = List();
  private var inNot = false;
  
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
