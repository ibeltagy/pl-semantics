package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import scala.collection.mutable.SetBuilder
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.mlnsemantics.wordnet.Wordnet
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import org.apache.commons.logging.LogFactory
import support.HardWeightedExpression

class FindEventsProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  private val LOG = LogFactory.getLog(classOf[FindEventsProbabilisticTheoremProver])

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {

    var newAssumption = assumptions.head.expression;
    var newGoal = goal;

    var eventVars = findEventVar(newAssumption);
    var propVars = findPropVar(newAssumption);
    newAssumption = convertToEvntPropVar(newAssumption);

    eventVars = findEventVar(newGoal);
    propVars = findPropVar(newGoal);
    newGoal = convertToEvntPropVar(newGoal);

    def convertToEvntPropVar(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerVariable(name) => {
          if (eventVars.contains(e))
            BoxerVariable("e" + name.substring(1))
          else if (propVars.contains(e))
            BoxerVariable("p" + name.substring(1))
          else
            return e
        }
        case _ => e.visitConstruct(convertToEvntPropVar)
      }
    }

    val newAssumptions = List(HardWeightedExpression(newAssumption))
    delegate.prove(constants, declarations, evidence, newAssumptions, newGoal)
  }

  private def findEventVar(e: BoxerExpression): List[BoxerVariable] = {
    e match {
      case BoxerRel(discId, indices, event, variable, name, sense) => {
        if (name == "agent" || name == "patient")
          return List(event);
        else
          return List();
      }
      case _ => {
        e.visit(findEventVar, (parts: List[List[BoxerVariable]]) => {
          var compined = List[BoxerVariable]();
          for (p <- parts)
            compined = compined ++ p;
          return compined.toSet.toList
        }, List())
      }
    }
  }

  private def findPropVar(e: BoxerExpression): List[BoxerVariable] = {
    e match {
      case BoxerProp(discId, indices, variable, drs) => return List(variable) ++ findPropVar(drs)
      case _ => {
        e.visit(findPropVar, (parts: List[List[BoxerVariable]]) => {
          var compined = List[BoxerVariable]();
          for (p <- parts)
            compined = compined ++ p;
          return compined.toSet.toList
        }, List())
      }
    }
  }

}
