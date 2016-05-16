package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.fol._
import utcompling.scalalogic.base.expression.BaseNegatedExpression

case class FolNegatedExpression(override val term: FolExpression)
    extends FolExpression
    with BaseNegatedExpression[FolExpression] {

    override def toString() =
        FolTokens.NOT + term

    override def _pretty(): List[String] = {
        val prettyterm = term._pretty
        return (FolTokens.NOT + prettyterm.head) +: prettyterm.tail.map("  "+_)
    }

}

object FolNegatedExpression {
}
