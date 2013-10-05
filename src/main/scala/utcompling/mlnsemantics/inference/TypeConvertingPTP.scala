package utcompling.mlnsemantics.inference

import utcompling.scalalogic.inference.TheoremProver
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SetBuilder
import scala.collection.mutable.HashMap
import utcompling.scalalogic.util.StringUtils._
import utcompling.scalalogic.util.Counter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.mlnsemantics.inference.support._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._

class TypeConvertingPTP(
  converter: BoxerExpressionInterpreter[FolExpression],
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {

    //val newConstants = constants.mapVals(_.map(converter.interpret))
    val newDeclarations = declarations.mapKeys(converter.interpret)
    val newEvidence = evidence.map(converter.interpret)
    val newGoal = converter.interpret(goal)
    val newAssumptions = assumptions.map {
      case HardWeightedExpression(e) => HardWeightedExpression(converter.interpret(e))
      case SoftWeightedExpression(e, w) => SoftWeightedExpression(converter.interpret(e), w)
    }

    delegate.prove(constants, newDeclarations, newEvidence, newAssumptions, newGoal)
  }

}
