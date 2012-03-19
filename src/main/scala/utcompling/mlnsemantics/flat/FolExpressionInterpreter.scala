package utcompling.mlnsemantics.flat

import utcompling.scalalogic.fol.expression.FolExpression

trait FolExpressionInterpreter {

  def interpret(e: FolExpression): FolExpression

}
