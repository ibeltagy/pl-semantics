package utcompling.mlnsemantics.flat

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable

class FlatPredicateQuotingInterpreter extends FolExpressionInterpreter {

    override def interpret(e: FolExpression): FolExpression =
        doInterpret(e, Set())

    private def doInterpret(e: FolExpression, bound: Set[String]): FolExpression =
        e match {
            case FolAtom(predicate, args @ _*) =>
                FolAtom(predicate, args.map(v => Variable(quote(v.name, bound))): _*)

            case FolVariableExpression(Variable(variable)) =>
                FolVariableExpression(Variable(quote(variable, bound)))

            case FolVariableBinderExpression(operator, variable, term) =>
                e.construct(List(variable, doInterpret(term, bound + variable.name)))

            case _ =>
                e.visitStructured(doInterpret(_, bound), e.construct)
        }

    private def quote(variable: String, bound: Set[String]) =
        if (bound.contains(variable))
            variable
        else if (variable.startsWith("\"") && variable.endsWith("\""))
            variable
        else
            '"' + variable + '"'

}
