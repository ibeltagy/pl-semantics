package utcompling.mlnsemantics.inference

import org.apache.commons.logging.LogFactory
import scala.io.Source
import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.fol.expression.FolAllExpression
import utcompling.scalalogic.fol.expression.FolAndExpression
import utcompling.scalalogic.fol.expression.FolAtom
import utcompling.scalalogic.fol.expression.FolEqualityExpression
import utcompling.scalalogic.fol.expression.FolExistsExpression
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.scalalogic.fol.expression.FolIfExpression
import utcompling.scalalogic.fol.expression.FolIffExpression
import utcompling.scalalogic.fol.expression.FolNegatedExpression
import utcompling.scalalogic.fol.expression.FolOrExpression
import utcompling.scalalogic.fol.expression.FolVariableExpression
import utcompling.scalalogic.top.expression.Variable
import opennlp.scalabha.util.FileUtils.pathjoin
import opennlp.scalabha.util.FileUtils
import utcompling.scalalogic.util.SubprocessCallable
import utcompling.mlnsemantics.inference.support._
import utcompling.scalalogic.inference.TheoremProver

class FakeProbabilisticTheoremProver[T, R](
  discreteTheoremProver: TheoremProver[T, R])
  extends ProbabilisticTheoremProver[T] {

  private val LOG = LogFactory.getLog(AlchemyTheoremProver.getClass)

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[T, Seq[String]],
    evidence: List[T],
    assumptions: List[WeightedExpression[T]],
    goal: T): Option[Double] = {

    // TODO: Add closed-word assumption using constants and declarations 

    discreteTheoremProver.prove(evidence ++ assumptions.map(_.expression), goal, LOG.isDebugEnabled)
      .map {
        case _ => 1. // if Some is returned 
      }

  }

}
