package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.drt._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.FolExpression
import scala.collection.mutable.ListBuffer
import utcompling.scalalogic.util.StringUtils
import utcompling.scalalogic.fol.expression.FolVariableExpression
import utcompling.scalalogic.fol.expression.FolParseExpression


case class DrtParseExpression(exps: List[(DrtExpression, Double)])
    extends DrtExpression {

    override def visit[S](function: DrtExpression => S, combinator: List[S] => S) =
        combinator(this.exps.map(  p=> function(p._1) ))

    //override def visitStructured[S](function: DrtExpression => S, combinator: List[Any] => S) =
      //combinator(this.exps.map(  p=> function(p._1) ))
        //combinator(List( this.exps.map(  p=>(function(p._1), p._2)   ) ))

    override def fol(): FolExpression = {
      return FolParseExpression(this.exps.map(e=>(e._1.fol, e._2)))
      
    }

    override def _folModal(world: Variable): FolExpression = {
      return FolParseExpression(this.exps.map(e=>(e._1._folModal(world), e._2))) 
    }
    
    override def getRefs(recursive: Boolean = false) = 
    	if (recursive)
            this.exps.map(_._1.getRefs(true)).flatten.toSet
        else
            Set();
            
    override def pretty(): String = {
        this.exps.map(_._1.pretty).mkString("\n")
    }

    override def toString(): String = {
        return "([%s])".format( this.exps.mkString(", "))
    }

}