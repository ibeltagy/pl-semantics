package utcompling.mlnsemantics.modal

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

class VisualizingModalTheoremProverDecorator[R](
  theoremProver: TheoremProver[BoxerExpression, R])
  extends TheoremProver[BoxerExpression, R] {

  private def d(drs: BoxerExpression) = new Boxer2DrtExpressionInterpreter().interpret(drs)

  override def prove(assumptions: List[BoxerExpression], goal: Option[BoxerExpression] = None, verbose: Boolean = false): Option[R] =
    theoremProver.prove(assumptions, goal, verbose)

  def proveVisualize(assumptions: List[BoxerExpression], goal: BoxerExpression, verbose: Boolean = false): (Option[R], String) = {
    val proof = this.prove(assumptions, goal, verbose)
    val parts =
      assumptions.map(d(_).pretty) :::
        //"   " ::
        //assumptions.map(a => box(f(a).pretty)) :::
        " = " + proof.isDefined + " => " ::
        //box(f(goal).pretty) ::
        //"   " ::
        d(goal).pretty ::
        Nil
    return (proof, sideBySideCentering(parts: _*))
  }

}
