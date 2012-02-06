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

class ModalTheoremProver[R](
    theoremProver: TheoremProver[FolExpression, R])
    extends TheoremProver[BoxerExpression, R] {

    private val p = new FolLogicParser().parse(_)
    private def d(drs: BoxerExpression) = new Boxer2DrtExpressionInterpreter().interpret(drs)
    private def f(drs: BoxerExpression) = d(drs).simplify.fol

    def prove(assumptions: List[BoxerExpression], goal: Option[BoxerExpression], verbose: Boolean = false): Option[R] = {
        //        assumptions.map(x => println(d(x).pretty))
        //        println(d(goal.get).pretty)

        val newAssumptions = assumptions.map(f)
        val newGoal = goal.map(f)

        if (theoremProver.prove(newAssumptions, p("P&-P"), verbose).isDefined) {
            throw new RuntimeException("INCONSISTENT")
        }

        theoremProver.prove(newAssumptions, newGoal, verbose)
    }

    def proveVisualizeFol(assumptions: List[FolExpression], goal: FolExpression, verbose: Boolean = false): (Option[R], String) = {
        val proof = theoremProver.prove(assumptions, goal, verbose)
        def pushNot(e: FolExpression): FolExpression = e match {
            //            case FolNegatedExpression(FolExistsExpression(v,t)) => FolAllExpression(v,pushNot(FolNegatedExpression(t)))
            //            case FolNegatedExpression(FolAllExpression(v,t)) => FolExistsExpression(v,pushNot(FolNegatedExpression(t)))
            //            case FolNegatedExpression(FolAndExpression(a,b)) => FolOrExpression(pushNot(FolNegatedExpression(a)), pushNot(FolNegatedExpression(b)))
            //            case FolNegatedExpression(FolOrExpression(a,b)) => FolAndExpression(pushNot(FolNegatedExpression(a)), pushNot(FolNegatedExpression(b)))
            case _ => e.visitStructured(pushNot, e.construct)
        }
        val parts =
            assumptions.map(a => box(pushNot(a).pretty)) :::
                " = " + proof.isDefined + " => " ::
                box(pushNot(goal).pretty) ::
                Nil
        return (proof, sideBySideCentering(parts: _*))
    }

}