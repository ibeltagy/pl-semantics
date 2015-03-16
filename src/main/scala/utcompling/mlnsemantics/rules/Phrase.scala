package utcompling.mlnsemantics.rules

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression


case class SimplePhrase(head:BoxerPred, tail: List[BoxerPred]) //All predicates in head and tail share the same variable name
{
  def getVariable = head.variable
  def getPos = head.pos
  
  def getBoxerExpressionsList: List[BoxerExpression] = 
  	return head +: tail 
}

case class RelationalPhrase(head:BoxerRel, tail: SimplePhrase) //The SimplePhrase has the same variable name of head.variable.
															//head.event could be anything
{
  def isPreposition = {
    val relName = head.name
    val nonPrep = List("agent", "patient", "topic", "role", "theme")
    !(nonPrep.contains(relName))
  }
  def getHeadVariable = head.event
  def getTailVariable = head.variable  

  def getBoxerExpressionsList: List[BoxerExpression] = 
  	return head +: tail.getBoxerExpressionsList 
}


sealed trait Phrase {
	def size: Int;
	def getBoxerExpressionList(i:Int):List[BoxerExpression];
}

case class NounPhrase(noun: SimplePhrase) extends Phrase {
	def size: Int = 
		return 1;

	def getBoxerExpressionList(i:Int):List[BoxerExpression] = 
	{
		assert (i >=0 && i < size)
		return noun.getBoxerExpressionsList;
	}
}

case class PrepPhrase(head: SimplePhrase, tail:List[RelationalPhrase]) extends Phrase{

	def size: Int = 
		return tail.length;

	def getBoxerExpressionList(i:Int):List[BoxerExpression] = 
	{
		assert (i >=0 && i < size)
		return head.getBoxerExpressionsList ++  tail(i).getBoxerExpressionsList;
	}
	
}

case class VerbPhrase(verb:SimplePhrase, subject:List[RelationalPhrase], obj:List[RelationalPhrase]) extends Phrase {

	def size: Int =
	{
		assert(subject.length == 1);
		return scala.math.max(obj.length, 1);
	}

	def getBoxerExpressionList(i:Int):List[BoxerExpression] = 
	{
		assert (i >=0 && i < size)
		return verb.getBoxerExpressionsList ++  subject.head.getBoxerExpressionsList ++ (if (obj.size == 0) List() else  obj(i).getBoxerExpressionsList);
	}
	
}


