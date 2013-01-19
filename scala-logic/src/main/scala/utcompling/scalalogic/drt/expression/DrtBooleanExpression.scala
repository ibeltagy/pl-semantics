package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.top.expression.Variable

abstract case class DrtBooleanExpression(override val first: DrtExpression, override val second: DrtExpression)
    extends DrtBinaryExpression(first, second) {

}
