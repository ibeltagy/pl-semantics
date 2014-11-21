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

    def clearName(name: String): String = //remove surrounding quotes -if any-
    {
        if(name.length() > 2 && name.charAt(0) == ''' && name.charAt(name.length() - 1) == ''')
          name.substring(1, name.length()-1);
        else name
    }
    
    def go(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerPred(discId, indices, variable, name, pos, sense) =>
          BoxerPred(discId, indices, variable, clearName(name), pos, sense)
        case BoxerRel(discId, indices, event, variable, name, sense) =>
          BoxerRel(discId, indices, event, variable, clearName(name), sense)          
        case BoxerNamed(discId, indices, variable, name, typ, sense) =>
          BoxerNamed(discId, indices, variable, clearName(name), typ, sense)
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
