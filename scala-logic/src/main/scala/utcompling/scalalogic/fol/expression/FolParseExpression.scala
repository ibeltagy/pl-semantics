package utcompling.scalalogic.fol.expression

import utcompling.scalalogic.fol._
import utcompling.scalalogic.base.expression.BaseNegatedExpression
import scala.collection.immutable.List

case class FolParseExpression(val exps: List[(FolExpression, Double)])
    extends FolExpression {

    override def toString() =
       "{"+exps.map(e => e._1.toString()).mkString("\n") + "}"

    override def _pretty(): List[String] = {
        return exps.flatMap(e => e._1._pretty())
    }
    
    override def visit[S](function: FolExpression => S, combinator: List[S] => S) =
      combinator(exps.map(e => function(e._1)))
      
    override def visitStructured[S](function: FolExpression => S, combinator: List[Any] => S): S =
      combinator(exps.map(e => (function(e._1), e._2)))

}

object FolListExpression {
}
