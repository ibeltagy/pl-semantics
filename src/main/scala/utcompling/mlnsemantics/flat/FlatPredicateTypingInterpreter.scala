package utcompling.mlnsemantics.flat

import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression._

class FlatPredicateTypingInterpreter extends FolExpressionInterpreter {

    private val VAR_RE = """^([pex])\d*|"([pex])\d*"$""".r

    override def interpret(e: FolExpression): FolExpression =
        e match {
            case PredAtom(label, cur, name, variable) =>
                FolAtom(Variable("pred_%s_".format(stuff(variable))), Variable(label), Variable(cur), Variable(name), Variable(variable))

            case NamedAtom(label, cur, name, variable) =>
                FolAtom(Variable("named_%s_".format(stuff(variable))), Variable(label), Variable(cur), Variable(name), Variable(variable))

            case RelAtom(label, cur, name, event, variable) =>
                FolAtom(Variable("rel_%s_%s_".format(stuff(event), stuff(variable))), Variable(label), Variable(cur), Variable(name), Variable(event), Variable(variable))

            case EqAtom(label, cur, x, y) => {
                require(stuff(x) == stuff(y), e)
                FolAtom(Variable("eq_%s_".format(stuff(x))), Variable(label), Variable(cur), Variable(x), Variable(y))
            }

            case _ => e.visitStructured(interpret, e.construct)
        }

    private def stuff(v: String): String =
        VAR_RE.findFirstMatchIn(v) match {
            case Some(t) => t.subgroups.filter(_ != null) match {
                case List("p") => "p"
                case List("e") => "e"
                case List(x) => "x"
            }
            case _ => throw new RuntimeException("Unable to match '%s' with regex '%s'".format(v, VAR_RE))
        }

}
