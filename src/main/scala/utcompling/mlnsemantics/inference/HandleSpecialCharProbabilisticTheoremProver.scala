package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import scala.collection.mutable.SetBuilder
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import org.apache.commons.logging.LogFactory
import support.HardWeightedExpression

class HandleSpecialCharProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  private val LOG = LogFactory.getLog(classOf[HandleSpecialCharProbabilisticTheoremProver])

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Seq[Double] = {

    def clearName(name: String, pos: String): String = 
    {
        var newName = name;

        //remove surrounding quotes -if any-
        if(newName.length() > 2 && newName.charAt(0) == ''' && newName.charAt(newName.length() - 1) == ''')
          newName.substring(1, newName.length()-1);

        //remove leading "-"
    	if(newName.charAt(0) == '-')
          newName = "pos" + newName

    	newName
    }
    
    def go(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerPred(discId, indices, variable, name, pos, sense) =>
          BoxerPred(discId, indices, variable, clearName(name, pos), pos, sense)
        case BoxerRel(discId, indices, event, variable, name, sense) =>
          BoxerRel(discId, indices, event, variable, clearName(name, "r"), sense)          
        case BoxerNamed(discId, indices, variable, name, typ, sense) =>
          BoxerNamed(discId, indices, variable, clearName(name, "n"), typ, sense)
        case _ => e.visitConstruct(go)
      }
    }

    delegate.prove(constants, declarations, evidence, 
      assumptions.map {
        case HardWeightedExpression(e, w) => HardWeightedExpression(go(e), w)
        case SoftWeightedExpression(e, w) => SoftWeightedExpression(go(e), w)
      }, go(goal))
  }
}
