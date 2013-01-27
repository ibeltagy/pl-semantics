package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol._

case class FolAllExpression(override val variable: Variable, override val term: FolExpression)
    extends FolQuantifiedExpression(FolTokens.ALL, variable, term) {

}

object FolAllExpression {
}
