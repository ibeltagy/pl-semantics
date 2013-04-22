package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.drt._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.FolExpression
import scala.collection.mutable.ListBuffer
import utcompling.scalalogic.util.StringUtils
import utcompling.scalalogic.fol.expression.FolVariableExpression


case class DrtParseExpression(exps: List[DrtExpression])
    extends DrtExpression {

    override def visit[S](function: DrtExpression => S, combinator: List[S] => S) =
        combinator(this.exps.map(function))

    override def visitStructured[S](function: DrtExpression => S, combinator: List[Any] => S) =
        combinator(List(this.exps.map(function)))

    override def fol(): FolExpression = {
      return this.exps.map(_.fol).reduceLeft(_ & _)
    }

    override def _folModal(world: Variable): FolExpression = {
      return this.exps.map(_._folModal(world)).reduceLeft(_ & _)
    }
    
    override def getRefs(recursive: Boolean = false) = 
    	if (recursive)
            this.exps.map(_.getRefs(true)).flatten.toSet
        else
            Set();
            
    override def pretty(): String = {
        this.exps.map(_.pretty).mkString("\n")
    }

    override def toString(): String = {
        return "([%s])".format( this.exps.mkString(", "))
    }

}