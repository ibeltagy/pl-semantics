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
}

class Clique (val rule:String, val weight: Double, val varVal:List[(String, String)] /*List(MRF variable, possible value)*/)
{
	override def toString(): String = rule + ", " + varVal.map(p => p._1 +"_"+ p._2).sorted.mkString(", ");
	override def equals(o: Any) =
	{
		o match
		{
			case that: Clique => this.toString == that.toString
			case _ => false
		}
	}
	override def hashCode = this.toString.hashCode()

}
class Assignment (val vals:Array[String], val weight: Double, val cliques:List[(Clique)])
{
	override def toString(): String = vals.toList.mkString(", ");
}
class GraphRules {
  private val LOG = LogFactory.getLog(classOf[GraphRules])

  var rules:ListBuffer[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] = null;
  var cliques:ListBuffer[Clique] = null;
  var entityIdCounter:Int = 0;

  def predToString(pred:BoxerPred) : String = 
  {
		pred.name + "-" + pred.pos + "-" + pred.indices.applyOrElse(0, "0")
  }
  
  //Generate all possible rules of the form: potentialMatchs(rhsFrom) shortestpath potentialMatchs(rhsTo)  => rhsFrom shortestpath rhsTo
  //if textFromListInput isDefined, then the rules are of the form: textFromListInput shortestpath potentialMatchs(rhsTo)  => rhsFrom shortestpath rhsTo
  //The second setting is useful to override the default Baseline.entityPotentialMatchs and replace it with a more strict set of potential matchs
  def hypEntityPairToRule (rhsFrom:String, rhsTo:String, textFromListInput:Option[Set[String]]) : Int = //returns number of rules added
  {
	val hypGraph = Baseline.hypGraph 
	val textGraph = Baseline.textGraph
	if (rhsFrom == rhsTo) //useless rule. It should not be generated in the first place
		return 0;
	val spRhs = hypGraph.get(rhsFrom) shortestPathTo hypGraph.get(rhsTo)
	if (spRhs.isEmpty)
		return 0;
	val rhsPred = spRhs.get.nodes.flatMap(Baseline.hypEntitiesMap(_)).toList
	val rhsVars = spRhs.get.nodes.map(_.value).toList
	//val rhsText = rhsPred.sortBy(_.indices.apply(0).wordIndex).map(predToString).mkString(" ");
	val rhsRel = spRhs.get.edges.map(_.label).toList
	
	var textFromList =	if (textFromListInput.isDefined) textFromListInput.get.toList
						else Baseline.entityPotentialMatchs(rhsFrom)
	val textToList = Baseline.entityPotentialMatchs(rhsTo)
	var additionalRulesCount = 0;
	for (textFrom <- textFromList)
	{
		for (textTo <- textToList)
		{
			if (!textGraph.contains(textFrom))
				println (textFrom)
			val nodeFrom = textGraph.get(textFrom)
			val nodeTo = textGraph.get(textTo)
			val spLhs = nodeFrom shortestPathTo nodeTo
			if (spLhs.isDefined && spLhs.get.weight <= Sts.opts.graphRuleLengthLimit)
			{
				val lhsPred = spLhs.get.nodes.flatMap(Baseline.textEntitiesMap(_)).toList
				val lhsRel = spLhs.get.edges.map(_.label).toList
				//val lhsText = lhsPred.sortBy(_.indices.apply(0).wordIndex).map(predToString).mkString(" ");
				
				val ruleLhs:List[BoxerExpression] = lhsPred.asInstanceOf[List[BoxerExpression]] ++ lhsRel.asInstanceOf[List[BoxerExpression]]
				//new copy of ruleRhs every iteration because it changes with ruleLhs
				var ruleRhs:List[BoxerExpression] = rhsPred.asInstanceOf[List[BoxerExpression]] ++ rhsRel.asInstanceOf[List[BoxerExpression]]
				def changeRhsVar (v:String) : String = 
				{
					if (v == rhsFrom)
						return textFrom
					else if (v == rhsTo)
						return textTo
					else {
						entityIdCounter =  entityIdCounter + 1;
						return "n" + entityIdCounter;
					}
						//throw new RuntimeException("not implemented yet") //new entity that does not exist in the text
				}
				val rhsVarsVals = rhsVars.map(v => (v, changeRhsVar(v)))//List((hypGraphNode, possible value))
				val rhsVarsValsMap = rhsVarsVals.toMap
				ruleRhs = ruleRhs.map(
				{
					case BoxerPred(discId, indices, variable, name, pos, sense) => 
						BoxerPred(discId, indices, BoxerVariable(rhsVarsValsMap(variable.name)), name, pos, sense)
					case BoxerRel(discId, indices, event, variable, name, sense) => 
						BoxerRel(discId, indices, BoxerVariable(rhsVarsValsMap(event.name)), BoxerVariable(rhsVarsValsMap(variable.name)), name, sense)
				})
				val ruleLhsDrs = Rules.boxerAtomsToBoxerDrs(ruleLhs.toSet);
				val ruleRhsDrs = Rules.boxerAtomsToBoxerDrs(ruleRhs.toSet);
				
				var (rw, path) = Baseline.ruleScore (spLhs.get, spRhs.get);
				
				cliques += new Clique(path, rw, rhsVarsVals)
				rules +=( (ruleLhsDrs, ruleRhsDrs, rw, RuleType.Implication) )
				additionalRulesCount = additionalRulesCount + 1;
			}
		}
	}
	return additionalRulesCount;
  }

  def getRule(text: BoxerExpression, hypothesis: BoxerExpression, ruleWeighter: RuleWeighter,
    vecspaceFactory: ((String => Boolean) => BowVectorSpace)) : List[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] =
  {
	if (Sts.opts.graphRules == 0)
		return List();

	rules = ListBuffer();
	cliques = ListBuffer();
	entityIdCounter = 0;
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
	if (Sts.opts.graphRules == 1)
	{
		graphRulesLvl1
		if (Sts.opts.baseline == "search")
		{
			mapInfer(cliques.toSet, Some(Sts.qaRightAnswer));
			val answer = mapInfer(cliques.toSet, None);
			Sts.qaAnswer = answer._1;
		}

	}
	else if (Sts.opts.graphRules == 2)
	{
		graphRulesLvl2
	}
	else throw new RuntimeException("Not supported graphRules level: " + Sts.opts.graphRules)
	
	return rules.toList;
  }
  
  def graphRulesLvl1 =
  {
	val hypGraph = Baseline.hypGraph
	hypGraph.edges.foreach( hypPath =>
	{
		val rhsRel = hypPath.label.asInstanceOf[BoxerRel]
		val rhsFromEntity = rhsRel.event.name
		val rhsToEntity = rhsRel.variable.name
		hypEntityPairToRule(rhsFromEntity, rhsToEntity, None)
	})
  }
  def graphRulesLvl2 =
  {
	val hypGraph = Baseline.hypGraph
	val textGraph = Baseline.textGraph
	val placeholderNode = Baseline.hypGraph.get( Baseline.hypPreds.filter ( _.name == "@placeholder" ).head.variable.name )
	val hypEntitiesSorted = Baseline.topologicalSort(placeholderNode).asInstanceOf[List[hypGraph.NodeT]];
	val hypFrom = hypEntitiesSorted.head; //first node is the placeholder
	var bestEntity = ""
	var bestEntityW = 0.0;
	//TODO: uncomment to unlock alignment algorithm 2
	val allCliques:collection.mutable.Set[Clique] = collection.mutable.Set()
	val posCliques:collection.mutable.Set[Clique] = collection.mutable.Set()
	(Sts.qaRightAnswer :: Sts.qaEntities.keys.toList.sorted.filterNot(e => e.equals("@placeholder") || e.equals(Sts.qaRightAnswer)).toList).foreach(ne =>  //for each named entity in the document
	{
		cliques.clear(); //remove all cliques 

		val anchorNodes = new scala.collection.mutable.HashSet[hypGraph.NodeT];
		anchorNodes.add(hypFrom) //add the placeholder entity to the anchor nodes. 

		hypEntitiesSorted.tail.foreach(hypTo =>  //loop over all other nodes
		{
			var hypSp = hypTo.shortestPathTo(hypFrom)
			if (!hypSp.isEmpty)
			{
				//loop over rhsSp.get.nodes.tail until an anchor node is found
				//it can loop until it reaches the placeholder
				val path = hypSp.get.nodes.toList
				assert (path.length >= 2)
				var currentIdx = 1;
				var continue = true;
				while(!anchorNodes.contains(path(currentIdx)) )
					currentIdx = currentIdx + 1;
	
				var potentialMatches = if (currentIdx == path.length - 1)    //TODO: uncomment to unlock alignment algorithm 2
					Some(Sts.qaEntities(ne).map(_.substring(1))) //remove the leading "h" in the variable name. I do not even know what this "h" is for
									//if the fromNode is the placeholder, then replace the set of
									//potential matches with entities of one named entity
				else None
	
				val count = hypEntityPairToRule(path(currentIdx), hypTo, potentialMatches)
				if (count > 0) //rules added for hypTo ==> hypTo is an anchor node 
					anchorNodes.add(hypTo)
			}
		})
		allCliques ++= cliques;
		val answer = mapInfer(cliques.toSet, Some(ne));
		
		if (Sts.qaRightAnswer == ne)
			posCliques ++= answer._3;
			
		if (answer._2 > bestEntityW)
		{
			bestEntity = answer._1
			bestEntityW = answer._2
		}
	})
	val allRulesSet = allCliques.map(_.rule).toSet;
	val posRulesSet = posCliques.map(_.rule).toSet;
	allRulesSet.diff(posRulesSet).foreach(x => println ("neg\t" + Sts.pairIndex + "\t"  + x))
	posRulesSet.foreach(x => println ("pos\t" + Sts.pairIndex + "\t"  + x))
	Sts.qaAnswer = bestEntity
  }
  def mapInfer(cliquesSet:Set[Clique], entity:Option[String]) : (String, Double, Set[Clique]) = 
  {
  	implicit def ordering[A <: Assignment]: Ordering[A] = new Ordering[A]
	{
		override def compare(x: A, y: A): Int = {
			x.weight.compareTo(y.weight)
		}
	}
	val q:PriorityQueue[Assignment] = PriorityQueue[Assignment]();
	
  	val placeholderNode = Baseline.hypGraph.get( Baseline.hypPreds.filter ( _.name == "@placeholder" ).head.variable.name )
	val varsSorted  = Baseline.topologicalSort(placeholderNode).map(_.value);
	LOG.trace("Topological sort of question entities: " + (varsSorted.mkString(", ")))
	LOG.trace("Number of rules: " + cliquesSet.size)
	//collect then print all possible values of each variable
	val varVals:collection.mutable.Map[String, collection.mutable.Set[String]] = collection.mutable.Map() ++ varsSorted.map(_->collection.mutable.Set[String]())
	val cliquesList  = cliquesSet.toList.sortWith((a, b) => { //sort it for reproducibility of runs
		if (a.weight == b.weight)
		{
			if (a.rule.length() == b.rule.length())
				a.rule < b.rule
			else a.rule.length() < b.rule.length()
		}
		else a.weight > b.weight
	})
	val cliquesListWithIndex = cliquesList.zipWithIndex
	cliquesListWithIndex.foreach( e => LOG.trace(( e._2 + " -- %1.3f".format(e._1.weight) + " -- " + e._1.varVal.mkString(", "))))
	val cliquesIndexHash = cliquesListWithIndex.toMap
	cliquesIndexHash.keys.foreach(c =>  c.varVal.foreach(v => {
		if (varVals.contains(v._1)) //all vars in cliques supposed to be in vars, but because some queries are 
								//disconnected, and because topological sort keeps just connected nodes,
								//it happens that some vars in varVals do not exist in vars. 
			varVals(v._1).add(v._2)
	}))
	LOG.trace("\n" + varVals.mkString("\n"));
	
	//initializing the queue with all entities
	val namedEntities = if (entity.isDefined)
		// initialize the queue with entities of the right answer only. This is to find rules for training the lexical entailment
		Baseline.textPreds.filter ( _.name == entity.get).map(_.variable.name).toList
	else
		//initialize the queue with all named entities
		Baseline.entityPotentialMatchs(placeholderNode.value) 
	
	namedEntities.sorted /*sorted for reproducibility of runs*/.map(v => {
		val tmp = new Array[String](varsSorted.length)//every possible assignment should have the same length as  varsSorted
		tmp(0) = v //set a value for the first variable in the assignment
		tmp
	}).foreach(assignment => q+= new Assignment(assignment, 0.0, List[Clique]()))

	var cnt = 0
	var bestAssignmentW = 0.0;
	var bestAssignmentCliques = List[Clique]();
	var bestEntityAssignment = new Array[String](varVals.size)

	while (!q.isEmpty)
	{
		val assignment = q.dequeue;
		LOG.trace(cnt + " -- " + "%1.4f".format(assignment.weight)
					  + " -- " + assignment.cliques.length
					  + " -- " + assignment.vals.toList.mkString(", ")
					  + " -- " + assignment.cliques.map(c => cliquesIndexHash(c)).mkString(", ") )
		
		//TODO: find all fillable leaves and fill them now because this local decision is globally 
		//optimal. Do not wait to try them as proposed assignments. 
		//TODO: suggestion above is wrong. Better use a recursive search algorithm
		//Comment above is true, but things are fast already, so no need for the optimization

		if (bestAssignmentW < assignment.weight )
		{
			bestAssignmentW = assignment.weight;
			bestAssignmentCliques = assignment.cliques
			bestEntityAssignment = assignment.vals
		}
		cnt = cnt + 1;
		
		val proposedAssignments = cliquesList.flatMap(c => {
			if (!assignment.cliques.contains(c)) //this clique has been applied to this assignment before. Do not use it again
			{
				var tmpAssignment:Array[String] = new Array[String](assignment.vals.length)
				Array.copy(assignment.vals, 0, tmpAssignment, 0, assignment.vals.length);
				var compatible = true;
				var attachementFound = false;
				var newVarIndex = -1
				var newVarAssignmentsCount = 0;
				c.varVal.foreach( varVal =>{ //for each variable-value pair in the clique
					val varIndex = varsSorted.indexOf(varVal._1); //get index of variable in the assignment (a hashmap would be faster) 
					if (varIndex > -1) // if the variable is not in varsSorted, do nothing. This happens if the hypGraph is not connected
					{
						if (tmpAssignment(varIndex) == null)
						{
							tmpAssignment(varIndex) = varVal._2  //add it to the tmp assignment
							newVarAssignmentsCount = newVarAssignmentsCount + 1;
							//assert (newVarIndex == -1) //all cliques are of length 2 
							if (newVarIndex != -1)
							{
								newVarIndex = Math.min(newVarIndex, varIndex); //Even though the clique will be assigning values to more than 
																			//one variable, we use the smallest index 
							}
							else newVarIndex = varIndex
						}
						else if (tmpAssignment(varIndex) == varVal._2)
							attachementFound = true  //at least one attachment point found
						else
							compatible = false;  //not compatible
					}
				})
				if (compatible && attachementFound)
					Some( (new Assignment(tmpAssignment, assignment.weight + /*newVarAssignmentsCount*/c.weight, assignment.cliques :+ c), newVarIndex) )
				else None
			}
			else None
		}).groupBy(_._2)  // group by the index of the new variable (or -1 if all variables already have values
		.toList   //change it to a list 
		.sortBy(_._1) //sort it by the index of the new variable
		.headOption.getOrElse( (0, List[(Assignment, Int)]() )) //get the top or an empty list 
		._2.unzip._1 //keep only the list of Assignments. Remove groupId, and remove newVarIndex. All of the assignments should be affecting the same variable 
		
		proposedAssignments.foreach(a => q.enqueue(a))

		/*
		//try to apply all cliques. A clique is applicable if it is connected to at least one value in the assignment
		//and this clique has not been applied before to this assignment
		//It does not matter if the clique proposes new values for variables or not. In case the clique does not propose 
		//new values, at least it will increase the score of the assignment
		
		//ProposedAssingments are generated by **reverse** order of cliques. This is very important to avoid 
		//generating the same combination of cliques more than once.
		val proposedAssignments = cliquesList.flatMap(c => {
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
							attachementFound = true  //at least one attachment point found
						else
							compatible = false;  //not compatible
					}
				})
				if (compatible && attachementFound)
					Some( (tmpAssignment, assignment._2 * Math.exp(c.weight), assignment._3 :+ c) )
				else None
			}
			else None
		}).toList
		//proposedAssignments.foreach(a => q.enqueue(a))
		for (i <- 0 until proposedAssignments.size)
		{
			var enqueu = true //enqueu by defualt unless a better compatible assingment exist
			//Check all other proposed assignments  
			for (j <- i+1 until proposedAssignments.size)
			{
				//compare proposedAssignments(i), proposedAssignments(j)
				//check if they are compatible or not
				var compatible = true;
				for (k <- 0 until proposedAssignments(i)._1.length)
				{
					if (proposedAssignments(i)._1(k) == proposedAssignments(j)._1(k)
						|| proposedAssignments(i)._1(k) == null
						|| proposedAssignments(j)._1(k) == null)
						None //still compatible
					else compatible = false
				}
				
				if (compatible)
				{
					//check which assignment comes first, i or j
					val ri = proposedAssignments(i)._3 diff proposedAssignments(j)._3 //the additional rule in i
					val rj = proposedAssignments(j)._3 diff proposedAssignments(i)._3 //the additional rule in j
					//ri and rj, each should be one Clique, the new clique added in this iteration
					assert(ri.length == rj.length && ri.length == 1)
					assert (ri.head != rj.head);
					if (cliquesIndexHash(ri.head) > cliquesIndexHash(rj.head))
					{
						enqueu = false;
						/*
						println("DROP" + " -- " + "%1.4f".format(proposedAssignments(i)._2)
										+ " -- " + proposedAssignments(i)._3.length
										+ " -- " + proposedAssignments(i)._1.toList.mkString(", ")
										+ " -- " + proposedAssignments(i)._3.map(c => cliquesIndexHash(c)).mkString(", ") )
						println("BCSF" + " -- " + "%1.4f".format(proposedAssignments(j)._2)
										+ " -- " + proposedAssignments(j)._3.length
										+ " -- " + proposedAssignments(j)._1.toList.mkString(", ")
										+ " -- " + proposedAssignments(j)._3.map(c => cliquesIndexHash(c)).mkString(", ") )
						*/
					}
				}
			}
			if (enqueu)
				q.enqueue(proposedAssignments(i)) 
		}
		*/
	}
	
	/*
	if (bestAssignmentW == 0)
	{
		if (entity.isDefined)
		{
			println  ("%s\t%s\t%d\t%1.4f\t%d\t%d\t%d\t%d".format(entity.get == Sts.qaRightAnswer, entity.get, varsSorted.length,
				0.0, 0, varsSorted.length, allRulesSet.size, 0))
			return (entity.get, 0)
		}
		else
			return (null, 0)
	}*/
	val answer:String = if (bestEntityAssignment.head == null)
	{
		if (entity.isDefined)
			entity.get
		else null
	}
	else
		Sts.qaEntities.filter(x => x._2.contains("h" + bestEntityAssignment.head)).toList.head._1
	if (entity.isDefined)
	{
		if (answer != entity.get)
			println("Could not infer the required entity")
		var nullCounter = 0;
		var newEntityCounter = 0;
		bestEntityAssignment.foreach( e => {
			if (e == null)
				nullCounter = nullCounter + 1;
			else if (e.startsWith("n")) 
				newEntityCounter = newEntityCounter + 1;
		})
		println  ("%d qid:%d 1:%d 2:%1.4f 3:%d 4:%d 5:%d 6:%d 7:%1.4f 8:%1.4f 9:%1.4f 10:%1.4f 11:%d #%s".format(
					if(entity.get == Sts.qaRightAnswer) 1 else 0,
					Sts.pairIndex,
					//entity.get,
					varsSorted.length,	//1
					bestAssignmentW,	//2
					newEntityCounter,	//3
					nullCounter,		//4 
					cliquesSet.size, 	//5 
					bestAssignmentCliques.size, 	//6
					if (varsSorted.length == nullCounter) 0.0 else 1.0*bestAssignmentW/(varsSorted.length - nullCounter),	//7
					1.0*newEntityCounter/varsSorted.length,	//8
					1.0*nullCounter/varsSorted.length,		//9
					1.0*bestAssignmentW/varsSorted.length,	//10
					if (bestEntityAssignment.head != null) 1 else 0, //11  //inferable entity
					entity.get
					))
		
	}
	//println ("Best rules:\n" + bestAssignmentCliques.mkString("\n"))
	//println ("Number of rules (%s): ".format(solved) + allRulesSet.size)
	//println ("Number of positive rules (%s): ".format(solved) + posRulesSet.size)
	//println ("Number of steps (%s): ".format(solved) + cnt)
	//println ("Best Weight (%s): ".format(solved) + "%1.4f".format(bestAssignmentW));
	//println ("Best Entity (%s): ".format(solved) + bestEntity + " = " + Sts.qaAnswer);
	return (answer, bestAssignmentW, bestAssignmentCliques.toSet)
  }
}

