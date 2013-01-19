package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.fol._
import utcompling.scalalogic.base.expression.BaseBinaryExpression

abstract case class FolBinaryExpression(override val first: FolExpression, override val second: FolExpression)
    extends FolExpression
    with BaseBinaryExpression[FolExpression] {

}
