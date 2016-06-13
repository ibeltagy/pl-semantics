package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.FileUtils
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.run.Sts
import scala.Array.canBuildFrom
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.mlnsemantics.datagen.Lemmatize
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.util.Resolution
import utcompling.mlnsemantics.util.InferenceRule
import utcompling.mlnsemantics.util.Literal
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerIndex
import utcompling.mlnsemantics.vecspace.{BowVector, BowVectorSpace}
import utcompling.mlnsemantics.inference.RuleWeighter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerNot
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerEq
import utcompling.mlnsemantics.inference.GivenNotTextProbabilisticTheoremProver
import utcompling.mlnsemantics.inference.Phase
import dhg.depparse.DepParser
import utcompling.mlnsemantics.datagen._
import utcompling.Resources
import scalax.collection.edge.WDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.mutable.Graph
import scalax.collection.edge.LUnDiEdge
import scalax.collection.edge.LkUnDiEdge
import org.ujmp.core.doublematrix.calculation.general.statistical.Var
import scala.collection.mutable.ListBuffer
import scala.math._
import utcompling.mlnsemantics.run.Baseline
import utcompling.mlnsemantics.inference.AwithCvecspaceWithSpellingSimilarityRuleWeighter
import utcompling.mlnsemantics.inference.SimpleCompositeVectorMaker
import scala.collection.mutable.PriorityQueue

object GraphRules {
  
  def main(args: Array[String]) {
		
	  //val hypGraph =  scalax.collection.mutable.Graph("cool", "hi","cool"~"hi");
    /*
      val hypGraph =  scalax.collection.mutable.Graph[String, LUnDiEdge]();
      println(hypGraph.contains(("1" ~+ "2")("label1")))
	  hypGraph += ("1" ~+ "2")("label2")
	  println(hypGraph.contains(("1" ~+ "2")("label1")))
      hypGraph += ("1" ~+ "2")("label1")
      println(hypGraph.contains(("1" ~+# "2")("label1")))
	  hypGraph += ("3" ~+ "2")("label2")
      hypGraph += ("3" ~+ "2")("label1")
	  hypGraph += "cool"
	  println (hypGraph)
	  * 
	  */
    /*
		val g = Graph(1~2 % 4, 2~3 % 2, 1~>3 % 5, 1~5  % 3,
		              3~5 % 2, 3~4 % 1, 4~>4 % 1, 4~>5 % 0)
		def n(outer: Int): g.NodeT = g get outer  // look up a node known to be contained
		 
		println( n(1) findSuccessor (_.outDegree >  3) )// Option[g.NodeT] = None 
		println( n(1) findSuccessor (_.outDegree >= 3) )// Option[g.NodeT] = Some(3)
		println( n(4) findSuccessor (_.edges forall (_.undirected))  )// Some(2)
		println( n(4) isPredecessorOf n(1)         )    // true 
		println( n(1) pathTo n(4)                  )    // Some(Path(1, 1~>3 %5, 3, 3~4 %1, 4))
		println( n(1) pathUntil (_.outDegree >= 3) )     // Some(Path(1, 1~>3 %5, 3))
		 
		val spO = g.get(1) shortestPathTo g.get(4) // Path(3, 3~4 %1, 4, 4~>5 %0, 5, 1~5 %3, 1)
		val sp = spO.get                   // here we know spO is defined
		println(sp.nodes)                           // List[g.NodeT] = Nodes(3, 4, 5, 1)
		println(sp.weight)                          // Long = 4
		println(sp.edges)
		println(sp)
		 
		def negWeight(e: g.EdgeT): Float = 5.5f - e.weight
		
		val spNO = n(3) shortestPathTo (n(1)) // Path(3, 2~3 %2, 2, 1~2 %4, 1)
		val spN = spNO.get                        // here we know spNO is defined
		println(spN.weight)      
		*                           // Long = 6
		*/
  }
  val newEntity:String = "N";
}

class Clique (val rule:String, val weight: Double, val varVal:List[(String, String)] /*List(MRF variable, possible value)*/)
{
	override def toString(): String = rule;
}
class GraphRules {
  private val LOG = LogFactory.getLog(classOf[GraphRules])
  
  def getRule(text: BoxerExpression, hypothesis: BoxerExpression, ruleWeighter: RuleWeighter,
    vecspaceFactory: ((String => Boolean) => BowVectorSpace)) : List[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] =
  {
  	if (Sts.opts.graphRules == 0)
  		return List();
  	val vectorspace = vecspaceFactory( _ => true );

	//Rules from entity to the first word
	/*
	val tmp = hypPreds.filter(_.name.contains("@placeholder")).map(_.variable.name);
	if (tmp.size != 1) //
		return List()
	var rhsFromEntity:String = tmp.head
	val firstHopEntities = hypRels.filter( r => Set(r.event.name, r.variable.name).contains(rhsFromEntity) )
			.flatMap(r => List(r.event.name, r.variable.name)).toSet - rhsFromEntity
	val textFromList = entityPotentialMatchs(rhsFromEntity)
	firstHopEntities.foreach(rhsToEntity => 
	{ 
	*/
  	val rules:ListBuffer[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] = ListBuffer();
  	val cliques:ListBuffer[Clique] = ListBuffer();
	//Rules for all edges in hypothesis
  	val hypGraph = Baseline.hypGraph 
  	val textGraph = Baseline.textGraph
  	
	hypGraph.edges.foreach( hypPath =>
	{
		val rhsRel = hypPath.label.asInstanceOf[BoxerRel]
		val rhsFromEntity = rhsRel.event.name
		val rhsToEntity = rhsRel.variable.name
		val rhsPred = List(rhsFromEntity, rhsToEntity).flatMap(Baseline.hypEntitiesMap(_))
		val textFromList = Baseline.entityPotentialMatchs(rhsFromEntity)
	//== 

		val textToList = Baseline.entityPotentialMatchs(rhsToEntity)
		for (textFrom <- textFromList)
		{
			for (textTo <- textToList)
			{
				//println (textFrom + " -- " + textTo)
				val nodeFrom = textGraph.get(textFrom)
				val nodeTo = textGraph.get(textTo)
				val spLhs = nodeFrom shortestPathTo nodeTo
				if (spLhs.isDefined && spLhs.get.weight <= Sts.opts.graphRuleLengthLimit)
				{
					//println ("Path found: " + spLhs)
					//println(spLhs.get.nodes.flatMap(textEntitiesMap(_)))
					//println(spLhs.get.weight)
					//println(spLhs.get.edges.map(_.label))
					def predToString(pred:BoxerPred) : String = 
					{
						pred.name + "-" + pred.pos + "-" + pred.indices.applyOrElse(0, "0")
					}
					val lhsPred = spLhs.get.nodes.flatMap(Baseline.textEntitiesMap(_)).toList
					val lhsText = lhsPred.sortBy(_.indices.apply(0).wordIndex).map(predToString).mkString(" ");
					val lhsRel = spLhs.get.edges.map(_.label).toList
					val ruleLhs:List[BoxerExpression] = lhsPred.asInstanceOf[List[BoxerExpression]] ++ lhsRel.asInstanceOf[List[BoxerExpression]]
					val spRhs = hypGraph.get(rhsFromEntity) shortestPathTo hypGraph.get(rhsToEntity)
					val rhsPred = spRhs.get.nodes.flatMap(Baseline.hypEntitiesMap(_)).toList
					val rhsVars = spRhs.get.nodes.map(_.value).toList
					val rhsText = rhsPred.sortBy(_.indices.apply(0).wordIndex).map(predToString).mkString(" ");
					val rhsRel = spRhs.get.edges.map(_.label).toList
					
					var ruleRhs:List[BoxerExpression] = rhsPred.asInstanceOf[List[BoxerExpression]] ++ rhsRel.asInstanceOf[List[BoxerExpression]]
					def changeRhsVar (v:String) : String = 
					{
						if (v == rhsFromEntity)
							return textFrom
						else if (v == rhsToEntity)
							return textTo
						else return GraphRules.newEntity //new entity that does not exist in the text 
					}
					ruleRhs = ruleRhs.map(
					{
						case BoxerPred(discId, indices, variable, name, pos, sense) => 
							BoxerPred(discId, indices, BoxerVariable(changeRhsVar(variable.name)), name, pos, sense)
						case BoxerRel(discId, indices, event, variable, name, sense) => 
							BoxerRel(discId, indices, BoxerVariable(changeRhsVar(event.name)), BoxerVariable(changeRhsVar(variable.name)), name, sense)
					})
					val ruleLhsDrs = Rules.boxerAtomsToBoxerDrs(ruleLhs.toSet);
					val ruleRhsDrs = Rules.boxerAtomsToBoxerDrs(ruleRhs.toSet);
					var (rw, path) = Baseline.ruleScore (spLhs.get, spRhs.get);
					val rhsVarsVals = rhsVars.map(v => (v, changeRhsVar(v)))//List((hypGraphNode, possible value))
					//println (rw + " -- " + rhsVarsVals)
					cliques += new Clique(path, rw, rhsVarsVals)
					//if (vectorspace.numDims > 0) // a vectorspace is provided
					//	rw = ruleWeighter.weightForRules(lhsText, List(), Seq((rhsText, List())).toMap, vectorspace).head._2.get; //rule weight based on dist sim
					//println ("GraphRule ("+spLhs.get.weight +  ", " + rw + ") " + lhsText + " -> " + rhsText);
					//(ruleRhs + " <<< " + ruleLhs)
					rules +=( (ruleLhsDrs, ruleRhsDrs, rw, RuleType.Implication) )
				}
			}
		}
	})
	if (Sts.opts.baseline == "search")
		mapInfer(cliques.toSet);

	return rules.toList;
  }

  val beamSize = 100;
  
  def mapInfer(cliques:Set[Clique]) = 
  {
	
  	implicit def ordering[A <: (Array[String], Double, List[Clique])]: Ordering[A] = new Ordering[A]
	{
		override def compare(x: A, y: A): Int = {
			x._2.compareTo(y._2)
		}
	}
	val q:PriorityQueue[(Array[String], Double, List[Clique])] = PriorityQueue[(Array[String], Double, List[Clique])]();
	
  	val placeholderNode = Baseline.hypGraph.get( Baseline.hypPreds.filter ( _.name == "@placeholder" ).head.variable.name )
	val varsSorted  = Baseline.topologicalSort(placeholderNode).map(_.value);
	LOG.trace("Topological sort of question entities: " + (varsSorted.mkString(", ")))
	//initializing the queue with all entities
	Baseline.entityPotentialMatchs(placeholderNode.value).map(v => {
		val tmp = new Array[String](varsSorted.length)//every possible assignment should have the same length as  varsSorted
		tmp(0) = v //set a value for the first variable in the assingment
		tmp
	}).foreach(assignment => q+= ((assignment, 1.0, List())))

	var cnt = 0
	var bestAssignmentW = 0.0;
	var bestAssignmentCliques = List[Clique]();
	var bestEntity = "";

	while (!q.isEmpty)
	{
		val assignment = q.dequeue;
		LOG.trace(cnt + " -- " + "%1.4f".format(assignment._2) + " -- " + assignment._3.length + " -- " + assignment._1.toList.mkString(", ")  )
		if (bestAssignmentW < assignment._2 )
		{
			bestAssignmentW = assignment._2;
			bestAssignmentCliques = assignment._3
			bestEntity = assignment._1.head
		}
		cnt = cnt + 1;
		
		//try to apply all cliques. A clique is applicable if it is connected to at least one value in the assignment
		//and this clique has not been applied before to this assignment
		//it does not matter if the clique proposes new values for variables or not. In case the clique does not propose 
		//new values, at least it will increase the score of the assignment

		cliques.foreach(c => {
			if (!assignment._3.contains(c)) //this clique has been applied to this assignment before. Do not use it again
			{
				var tmpAssignment:Array[String] = new Array[String](assignment._1.length)
				Array.copy(assignment._1, 0, tmpAssignment, 0, assignment._1.length);
				var compatible = true;
				var attachementFound = false;
				c.varVal.foreach( varVal =>{ //for each variable-value pair in the clique
					val varIndex = varsSorted.indexOf(varVal._1); //get index of variable in the assignment (a hashmap would be faster) 
					if (varIndex > -1) // if the variable is not in varsSorted, do nothing. This happens of the hypGraph is not connected
					{
						if (tmpAssignment(varIndex) == null)
							tmpAssignment(varIndex) = varVal._2  //add it to the tmp assignment
						else if (tmpAssignment(varIndex) == varVal._2)
							attachementFound = true  //at least one attachement point found
						else
							compatible = false;  //not compatible
					}
				})
				if (compatible && attachementFound)
					q.enqueue( (tmpAssignment, assignment._2 * Math.exp(c.weight), assignment._3 :+ c) )
			}
		})
	}
	val matchingEnt = Sts.qaEntities.filter(x => x._2 == "h" + bestEntity).toList
	Sts.qaAnswer = matchingEnt.head._1
	cliques.diff(bestAssignmentCliques.toSet).foreach(x => println ("neg - " + x.rule))
	bestAssignmentCliques.foreach(x => println ("pos - " + x.rule))
	//println ("Best rules:\n" + bestAssignmentCliques.mkString("\n"))
	println ("Number of rules: " + cliques.size)
	println ("Number of positive rules: " + bestAssignmentCliques.size)
	println ("Number of steps: " + cnt)
	println ("Best Weight: " + "%1.4f".format(bestAssignmentW));
	println ("Best Entity: " + bestEntity + " = " + Sts.qaAnswer);
  }
}

