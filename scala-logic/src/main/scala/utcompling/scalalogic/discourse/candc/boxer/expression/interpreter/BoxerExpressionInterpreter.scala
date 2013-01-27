package utcompling.scalalogic.discourse.candc.boxer.expression.interpreter

import utcompling.scalalogic.base.expression.BaseExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression

abstract class BoxerExpressionInterpreter[T] {

    def interpret(ex: BoxerExpression): T
    
}