package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol._

case class FolExistsExpression(override val variable: Variable, override val term: FolExpression)
    extends FolQuantifiedExpression(FolTokens.EXISTS, variable, term) {

}

object FolExistsExpression {
}
