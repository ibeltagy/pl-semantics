package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.drt._
import scala.collection.mutable.ListBuffer
import utcompling.scalalogic.base.expression.BaseApplicationExpression
import utcompling.scalalogic.fol.expression.{ FolExpression, FolVariableExpression }
import utcompling.scalalogic.util.StringUtils

case class DrtApplicationExpression(val function: DrtExpression, val argument: DrtExpression, val consequent: Option[DrtExpression] = None)
    extends DrtExpression
    with BaseApplicationExpression[DrtExpression] {

    override def visit[S](function: DrtExpression => S, combinator: List[S] => S) =
        combinator(List(function(this.function), function(this.argument)) ++ this.consequent.map(function))

    override def visitStructured[S](function: DrtExpression => S, combinator: List[Any] => S) =
        combinator(List(function(this.function), function(this.argument), this.consequent.map(function)))

    override def fol(): FolExpression = {
        val e = this.function.fol()(this.argument.fol())
        return this.consequent.map(c => e -> c.fol()).getOrElse(e)
    }

    override def _folModal(world: Variable): FolExpression = {
        val pred = this.pred._folModal(world)
        val args = this.args.map(_._folModal(world))
        val newArg = if (this.isAtom) List(FolVariableExpression(world)) else List()
        val e = pred(newArg ++ args: _*)
        return this.consequent.map(c => e -> c.fol()).getOrElse(e)
    }

    override def constants() =
        super.constants() ++ this.consequent.map(_.constants()).toSet.flatten

    override def predicates() =
        super.predicates() ++ this.consequent.map(_.predicates()).toSet.flatten

    override def simplify(): DrtExpression = {
        val function = this.function.simplify()
        val argument = this.argument.simplify()
        return function match {
            case le: DrtLambdaExpression => {
                val e = le.term.replace(le.variable, argument, false, true).simplify()
                this.consequent.map(_.simplify()) match {
                    case Some(c) => {
                        e match {
                            case app: DrtApplicationExpression => DrtApplicationExpression(app.function, app.argument, Some(c))
                            case drs: DrtBoxExpression => DrtBoxExpression(drs.refs, drs.conds, Some(c))
                            case con: DrtConcatenationExpression => DrtConcatenationExpression(con.first, con.second, Some(c))
                        }
                    }
                    case None => e
                }
            }
            case _ => this.construct(List(function, argument, consequent))
        }
    }

    override def getRefs(recursive: Boolean = false) =
        if (recursive)
            this.function.getRefs(true) ++ this.argument.getRefs(true) ++ this.consequent.map(_.getRefs(true)).toSet.flatten
        else
            Set()

    override def toString(): String = {
        val supString = super.toString
        return this.consequent.map(c => "(%s -> %s)".format(supString, c.toString)).getOrElse(supString)
    }

    override def pretty(): String = {
        // uncurry the arguments and find the base function
        var (function, args) =
            if (this.isAtom())
                (this.pred, this.args)
            else
                (this.function, List(this.argument)) //Leave arguments curried

        val functionParts =
            if (shouldParenthesizeFunction(function))
                StringUtils.sideBySideCentering(DrtTokens.OPEN, function.pretty, DrtTokens.CLOSE)
            else
                function.pretty

        val antecedent = StringUtils.sideBySideCentering((functionParts :: DrtTokens.OPEN :: args.map(_.pretty).flatMap(List(", ", _)).tail ::: DrtTokens.CLOSE :: Nil):_*)

        return this.consequent.map(c => StringUtils.sideBySideCentering(DrtTokens.OPEN, antecedent, " -> ", c.pretty, DrtTokens.CLOSE)).getOrElse(antecedent)
    }

}

object DrtApplicationExpression {
}
