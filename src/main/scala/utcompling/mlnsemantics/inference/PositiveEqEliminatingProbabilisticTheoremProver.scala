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
  /**
   * Assuming:
   * --negated equality expressions are like !eq(x1,x2) and it is never
   * !( eq(x1,x2) ^ ..... ^ ... )
   * --equality expressions are between variables, not formulas
   * --no duplicate equality expressions
   * --equality expressions like x1 = x2 ^ x2 = x3 will cause a problem, but
   * we do not care. Let's hope this case does not happen
   */
  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {

    object AllDone extends Exception { }
    
    def groupEqvClasses(eq: List[Set[String]]): List[Set[String]] =
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
    
    LOG.trace("DRSs before changes: ")	
    
    inNot = false;
    var assumEqualities = findEq(assumptions.head.expression)//.map(x => (x.head -> x));
    equalities = groupEqvClasses(assumEqualities);
    val newAssumption = removeEq(assumptions.head.expression);
    LOG.trace("\n" + new Boxer2DrtExpressionInterpreter().interpret(assumptions.head.expression).pretty)    
    LOG.trace(equalities) 

    val newAssumptions = List(HardWeightedExpression(newAssumption)) ++
      assumptions.filterNot(_ == assumptions.head);
    
    inNot = false;
    var goalEqualities = findEq(goal); 
    equalities  = groupEqvClasses(goalEqualities);
    val newGoal = removeEq(goal);
    LOG.trace("\n" + new Boxer2DrtExpressionInterpreter().interpret(goal).pretty)    
    LOG.trace(equalities)    

    delegate.prove(
      constants,
      declarations,
      evidence,
      newAssumptions,
      newGoal)
  }
 
  //private var equalities:Map[String,Set[String]] = Map();
  private var equalities:List[Set[String]] = List();
  private var inNot = false;
  
  
  private def findEq(e: BoxerExpression): List[Set[String]] = {
    e match {
      case BoxerNot(discId, indices, drs) => {
        inNot = true;
        findEq(drs)
      }
      case BoxerDrs(refs, conds) => {
        val currentInNot = inNot;  //use a local copy 
        inNot = false; //for the recursive call, always say not in NOT
        if (conds.size == 1 && currentInNot && conds.head.isInstanceOf[BoxerEq]) 
          List(); //negated equality
        else
        { //if equality found, it should be non-negated. 
          conds.flatMap(c=> {
           if (c.isInstanceOf[BoxerEq] && currentInNot)//equality under NOT in a list of conditions longer than 1
        	   throw new RuntimeException("Un-supported DRS: negated DRS that has an EQ with other predicates");
           findEq(c) //apply findEq to the condition
          })
        }
      }
      case BoxerEq(discId, indices, first, second) => {
          List(Set(first.name, second.name)) //called only for non-negated equalities
      }
      case _ => {
        e.visit(findEq, (parts: List[List[Set[String]]]) => parts.flatten, List.empty[Set[String]])
      }
    }
  }

  private def removeEq(e: BoxerExpression): BoxerExpression = {
    e match {
      case BoxerAlfa(variable, first, second) => 
        BoxerAlfa(removeEq(variable), removeEq(first), removeEq(second)); 

      case BoxerCard(discId, indices, variable, num, typ) =>
        BoxerCard(discId, indices, removeEq(variable), num, typ)
      

      case BoxerNot(discId, indices, drs) => {
    	  inNot = true;  
    	  BoxerNot(discId, indices, removeEq(drs))
      }      
      case BoxerDrs(ref, cond) =>{
        val currentInNot = inNot;  //use a local copy 
        inNot = false; //for the recursive call, always say not in NOT
        val conditions = 
        ( 
	        if (cond.size == 1 && currentInNot && cond.head.isInstanceOf[BoxerEq]) 
	          cond //negated equality
	        else if (cond.size == 1 && !currentInNot && cond.head.isInstanceOf[BoxerEq]) 
	          throw new RuntimeException("Un-supported DRS: can not handle drs of size one where this one predicate is an equality expression");
	        else
	        { //if equality found, it should be non-negated. 
	          cond.flatMap(c=> {
	           if (c.isInstanceOf[BoxerEq] && currentInNot)//equality under NOT in a list of conditions longer than 1
	        	   throw new RuntimeException("Un-supported DRS: negated DRS that has an EQ with other predicates");
	           else if (c.isInstanceOf[BoxerEq])
	        	   List() //remove equality expression 
	           else 
	        	   List(removeEq(c)) //apply findEq to the condition
	          })
	        }
	    )
	    if(conditions.size == 0)
	    	throw new RuntimeException("Un-supported DRS: after removing BoxerEq, remains an empty DRS");
        BoxerDrs(ref.map((listRef: (List[BoxerIndex], BoxerVariable)) => {
          (listRef._1, removeEq(listRef._2))
        }), conditions);
      }
      case BoxerEq(discId, indices, first, second) => throw new RuntimeException("Should not call removeEq on BoxerEq")
      
      
      case BoxerNamed(discId, indices, variable, name, typ, sense) => 
        BoxerNamed(discId, indices, removeEq(variable), name, typ, sense)
      
      case BoxerPred(discId, indices, variable, name, pos, sense) =>
        BoxerPred(discId, indices, removeEq(variable), name, pos, sense)
      
      case BoxerProp(discId, indices, variable, drs) =>
        BoxerProp(discId, indices, removeEq(variable), removeEq(drs))
      
      case BoxerRel(discId, indices, event, variable, name, sense) =>
        BoxerRel(discId, indices, removeEq(event), removeEq(variable), name, sense);
      
      case BoxerTimex(discId, indices, variable, timeExp) => 
        BoxerTimex(discId, indices, removeEq(variable), timeExp);
      
      case _ => e.visitConstruct(removeEq)
    }
  }
  
  def removeEq(v: BoxerVariable): BoxerVariable = 
  {
  	equalities.foreach(eq => {
  		if (eq.contains(v.name))
  		  return BoxerVariable(eq.head);
  	})
	return v;
  }
}
