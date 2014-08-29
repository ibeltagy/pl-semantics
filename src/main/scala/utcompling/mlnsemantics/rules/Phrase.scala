package utcompling.mlnsemantics.inference.support

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel


case class SimplePhrase(head:BoxerPred, tail: List[BoxerPred]) //All predicates in head and tail share the same variable name
{
  def getVariable = head.variable
  def getPos = head.pos  
}

case class RelationalPhrase(head:BoxerRel, tail: SimplePhrase) //The SimplePhrase has the same variable name of head.variable.
															//head.event could be anything
{
}


sealed trait Phrase {
}

case class NounPhrase(noun: SimplePhrase) extends Phrase {
}

case class PrepPhrase(head: SimplePhrase, tail:List[RelationalPhrase]) extends Phrase{  
}

case class VerbPhrase(verb:SimplePhrase, subject:List[RelationalPhrase], obj:List[RelationalPhrase]) extends Phrase {
}


