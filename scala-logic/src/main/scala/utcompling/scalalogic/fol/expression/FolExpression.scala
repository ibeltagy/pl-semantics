package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.base.expression.BaseExpression
import utcompling.scalalogic.top.expression.Expression
import utcompling.scalalogic.util._
import utcompling.scalalogic.fol._

abstract case class FolExpression
  extends Expression
  with BaseExpression[FolExpression] {

  def unary_- = FolNegatedExpression(this)
  def &(other: FolExpression) = FolAndExpression(this, other)
  def |(other: FolExpression) = FolOrExpression(this, other)
  def ->(other: FolExpression) = FolIfExpression(this, other)
  def <->(other: FolExpression) = FolIffExpression(this, other)
  def all(vars: Variable*): FolExpression = this all vars
  def all(vars: TraversableOnce[Variable]): FolExpression = vars.foldRight(this)(FolAllExpression(_, _))
  def exists(vars: Variable*): FolExpression = this exists vars
  def exists(vars: TraversableOnce[Variable]): FolExpression = vars.foldRight(this)(FolExistsExpression(_, _))
  def lambda(vars: Variable*): FolExpression = this lambda vars
  def lambda(vars: TraversableOnce[Variable]): FolExpression = vars.foldRight(this)(FolLambdaExpression(_, _))

  override def applyto(other: FolExpression): FolExpression =
    FolApplicationExpression(this, other)

  override def makeVariableExpression(variable: Variable): FolVariableExpression =
    FolVariableExpression(variable)

  def pprint() =
    println(this.pretty())

  def pretty() =
    this._pretty().mkString("\n")

  def _pretty(): List[String] =
    List(this.toString)

  //get all variables in the expression. Ignore universally/existentially variables that are not used in predicates 
  def getVariables(): Set[Variable] = getVariables(this).toSet;
  private def getVariables(e: FolExpression): Seq[Variable] =
    e match {
      case FolAtom(pred, args @ _*) => args;
      case FolVariableExpression(v) => Seq(v);
      case _ => e.visit(getVariables, (parts: List[Seq[Variable]]) => parts.flatten)
    }

}
