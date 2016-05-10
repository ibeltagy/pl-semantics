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
		println(spN.weight)                                // Long = 6
  }
  	
  var isTextNegated:Boolean = false; //set them when GivenNotTextProbabilisticTheoremProver.phase == Phase.notHGivenT
  var isHypNegated:Boolean = false;  //then use them when GivenNotTextProbabilisticTheoremProver.phase == Phase.hGivenT
  var allGeneratedRules:List[(String, String)] = List[(String, String)]();  //List("gs + notSure + lhs + rhs",   rule )
  var ruleID:Int = 1;

  def printDiffRules (s:String) = 
  {
    try
    {
	    if (Sts.opts.printDiffRules)
	      println(s)
    }catch {
      case _ =>
    }
  }
  printDiffRules("[pattern]" + "\t" + "ruleID" + "\t" + "lhsText" + "\t" + "rhsText"+ "\t" + "w" + "\t"+ "gsw" +"\t" + "notSure" + "\t" + "isInWordnet" + "\t" + "ruleEval" + "\t" + "extentionLevel" + "\t" + "pairIndex" + "\t" + "text" + "\t" + "hypothesis" + "\t" + "lhsDrs" + "\t" + "rhsDrs")

	//if(Sts.depParser == null)
	//	Sts.depParser = DepParser.load();

	val sureRulesFile = Resources.sureRules
	val sureRules:Set[String] = FileUtils.readLines(sureRulesFile).toSet
	val trueRulesFile = Resources.trueRules
	val trueRules:Set[String] = FileUtils.readLines(trueRulesFile).toSet
}

class GraphRules {
  private val LOG = LogFactory.getLog(classOf[GraphRules])
  object AllDone extends Exception { }
  object HalfDone extends Exception { } 
  // Search phrases in Text-Hypothesis pair
  
  var lhsSentence:String = "";
  var rhsSentence:String = "";
  var textAtomsMap:Map[String, (BoxerExpression, String)] = null;
  var hypAtomsMap:Map[String, (BoxerExpression, String)] = null;
  var ruleWeighter:RuleWeighter = null;
  var vectorspace:BowVectorSpace = null;


  def getRule(text: BoxerExpression, hypothesis: BoxerExpression, ruleWeighter: RuleWeighter,
    vecspaceFactory: ((String => Boolean) => BowVectorSpace)) : List[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] =
  {
  	if (!Sts.opts.graphRules && !Sts.opts.printDiffRules)
  		return List();
  	
  	GraphRules; //initialize the object in case it is not initialize yet.	
  	
  	//remove "indices" from predicates and relations because many duplicates only differ in the indices
  	val textPreds = text.getPredicates.map(
  	{	 
  		case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discId, List(), variable, name, pos, sense)
  	}).toSet.toList
  	val textRels = text.getRelations.map(
  	{ 
  		case BoxerRel(discId, indices, event, variable, name, sense) => BoxerRel(discId, List(), event, variable, name, sense)
  	}).toSet.toList
  	val hypPreds = hypothesis.getPredicates.map(
  	{ 
  		case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discId, List(), variable, name, pos, sense)
  	}).toSet.toList
  	val hypRels = hypothesis.getRelations.map(
  	{ 
  		case BoxerRel(discId, indices, event, variable, name, sense) => BoxerRel(discId, List(), event, variable, name, sense)
  	}).toSet.toList
  	
  	val tmp = hypPreds.filter(_.name.contains("@placeholder")).map(_.variable.name);
  	if (tmp.length != 1) //
  	  return List()
  	  
  	var queryEntity:String = tmp.head
  	
  	val textEntities = (textPreds.map(_.variable.name) ++ textRels.flatMap(r => List(r.variable.name, r.event.name))).toSet.toList
  	val textEntitiesMap = textEntities.map( e => (e -> textPreds.filter(_.variable.name == e).toSet )).toMap
  	
  	val hypEntities = (hypPreds.map(_.variable.name) ++ hypRels.flatMap(r => List(r.variable.name, r.event.name))).toSet.toList
  	val hypEntitiesMap = hypEntities.map( e => (e -> hypPreds.filter(_.variable.name == e).toSet )).toMap
  	
    //entities extracted from predicates should contain all entities in relations too
  	textRels.foreach(r =>{ 
  	  require(textEntities.contains(r.event.name), "Entity 1 %s is not in text %s".format(r.event, textEntities));
  	  require(textEntities.contains(r.variable.name), "Entity 2 %s is not in text %s".format(r.variable, textEntities));
  	})
  	hypRels.foreach(r =>{ 
  	  require(hypEntities.contains(r.event.name), "Entity %s is not in hypothesis  %s".format(r.event, hypEntities));
  	  require(hypEntities.contains(r.variable.name), "Entity %s is not in hypothesis %s".format(r.variable, hypEntities));
  	})
  	
  	//find potential matched entities 
  	val entityPotentialMatchs = hypEntities.map(hypE => (hypE -> textEntities.filter( textE => {
  	  //two entities match if they share a predicate with the same lemma
  	  val textWords = textEntitiesMap(textE).map(_.name)
  	  val hypWords = hypEntitiesMap(hypE).map(_.name)
  	  var x  = textWords intersect hypWords
  	  x = x -- Set("male", "female", "topic") //ignore meta words
  	  //if (hypWords != tmp)
  	  //  tmp = hypWords
  	  var y = Set[String]();

  	  if (hypWords.contains("'@placeholder'"))
  	    y = Sts.qaEntities.keySet intersect (textWords.map (w =>  {
    	  if (w.startsWith("'") && w.endsWith("'"))
    		  w.substring(1, w.length()-1);
    	  else
    	    w
  	    }))
  	  //println (hypWords.toString + " --- " + textWords.toString + " --- " + x.toString)
  	  ! (x.isEmpty && y.isEmpty)
  	  //TODO: add potential matches using WordNet, DistSim and LexicalEnt
  	} ))).toMap
  	
  	println(entityPotentialMatchs)

  	
  	val hypGraph = scalax.collection.mutable.Graph[String, LUnDiEdge]();
  	hypEntities.foreach( e => hypGraph += e)
  	hypRels.foreach( r => {
  	  val edge = (r.event.name~+r.variable.name)(r);
  	  require( !hypGraph.contains(edge), "Edge " + edge + " already exists in graph " + hypGraph)
  	  hypGraph += edge
  	});
  	println (hypGraph)
  	
  	val textGraph = scalax.collection.mutable.Graph[String, LUnDiEdge]();
  	textEntities.foreach( e => textGraph += e)
  	textRels.foreach( r => {
  	  val edge = (r.event.name~+r.variable.name)(r);
  	  	//Ignore this check because it is wrong, and because the code below will still work without it.
  	  //require( !textGraph.contains(edge), "Edge " + edge + " already exists in graph " + textGraph)
  	  textGraph += edge
  	});
  	println (textGraph)
  	
  	val firstHopEntities = hypRels.filter( r => Set(r.event.name, r.variable.name).contains(queryEntity) )
  			.flatMap(r => List(r.event.name, r.variable.name)).toSet - queryEntity
  	println (firstHopEntities)
  	
  	
  	val hypFrom = queryEntity
  	val textFromList = entityPotentialMatchs(hypFrom)
  	
  	val rules:ListBuffer[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] = ListBuffer();
  	
  	firstHopEntities.foreach(hypTo => 
  	{
  	  val textToList = entityPotentialMatchs(hypTo)
  	  for (textFrom <- textFromList)
  	  {
  		  for (textTo <- textToList)
  		  {
  			  println (textFrom + " -- " + textTo)
  			  val nodeFrom = textGraph.get(textFrom)
  			  val nodeTo = textGraph.get(textTo)
  			  val sp = nodeFrom shortestPathTo nodeTo
  			  if (sp.isDefined)
  			  {
  				  //println ("Path found: " + sp)
  				  //println(sp.get.nodes.flatMap(textEntitiesMap(_)))
  				  //println(sp.get.weight)
  				  //println(sp.get.edges.map(_.label))
  				  val lhsPred = sp.get.nodes.flatMap(textEntitiesMap(_))
  				  val lhsRel = sp.get.edges.map(_.label) 
  				  val ruleLhs:List[BoxerExpression] = lhsPred.asInstanceOf[List[BoxerExpression]] ++ lhsRel.asInstanceOf[List[BoxerExpression]]
  				  val spRhs = hypGraph.get(hypFrom) shortestPathTo hypGraph.get(hypTo)
  				  val rhsPred = spRhs.get.nodes.flatMap(hypEntitiesMap(_))
  				  val rhsRel = spRhs.get.edges.map(_.label)
  				  var ruleRhs:List[BoxerExpression] = rhsPred.asInstanceOf[List[BoxerExpression]] ++ rhsRel.asInstanceOf[List[BoxerExpression]]
  				  def changeRhsVar (v:BoxerVariable) : BoxerVariable = 
  				  {
  				    if (v.name == hypFrom)
  				      return BoxerVariable(textFrom)
  				    else if (v.name == hypTo)
  				      return BoxerVariable(textTo)
  				    else throw new RuntimeException("Variable not found " + v);
  				  }
  				  ruleRhs = ruleRhs.map({
					{
						case BoxerPred(discId, indices, variable, name, pos, sense) => 
							BoxerPred(discId, indices, changeRhsVar(variable), name, pos, sense)
						case BoxerRel(discId, indices, event, variable, name, sense) => 
							BoxerRel(discId, indices, changeRhsVar(event), changeRhsVar(variable), name, sense)
					}
  				  })
  				  val ruleLhsDrs = Rules.boxerAtomsToBoxerDrs(ruleLhs.toSet);
  				  val ruleRhsDrs = Rules.boxerAtomsToBoxerDrs(ruleRhs.toSet);
  				  println (ruleRhs + " <<< " + ruleLhs)
  				  rules +=  ( (ruleLhsDrs, ruleRhsDrs, 1.0/sp.get.weight, RuleType.Implication) )

  			  }
  		  }
  	  }
  	})
  	return rules.toList;
  	//def n(outer: String, graph): g.NodeT = g get outer  // look up a node known to be contained

  	/*
  	queryEntity = "x1001"
  	var foundBefore:List[Any] = List();
  	val queryNode = hypGraph.get(queryEntity);
  	var continue = true;
	println(queryNode)
  	while (continue)
  	{
	  	var connectedNode = queryNode findSuccessor (t => !foundBefore.contains(t))
	  	println(connectedNode)
	  	if (connectedNode.isEmpty)
	  	  continue = false;
	  	else 
	  		foundBefore = connectedNode.get :: foundBefore
  	}
  	*/
  	println("hi")
  	
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
	 
	val spO = n(3) shortestPathTo n(1) // Path(3, 3~4 %1, 4, 4~>5 %0, 5, 1~5 %3, 1)
	val sp = spO.get                   // here we know spO is defined
	println(sp.nodes)                           // List[g.NodeT] = Nodes(3, 4, 5, 1)
	println(sp.weight)                          // Long = 4
	 
	def negWeight(e: g.EdgeT): Float = 5.5f - e.weight
	
	val spNO = n(3) shortestPathTo (n(1)) // Path(3, 2~3 %2, 2, 1~2 %4, 1)
	val spN = spNO.get                        // here we know spNO is defined
	println(spN.weight)                                // Long = 6
	 */
	/*
	val pO1 = n(4).withSubgraph(nodes = _ < 4) pathTo n(2) // Some(Path(4, 3~4 %1, 3, 2~3 %2, 2))
	pO1.map(_.nodes)                                       // Some(Nodes(4, 3, 2))
	 
	val pO2 = n(4).withSubgraph(edges = _.weight != 2) pathTo n(2)
	                                   // Some(Path(4, 4~>5 %0, 5, 1~5 %3, 1, 1~2 %4, 2))
	pO2.map(_.nodes)                   // Some(Nodes(4, 5, 1, 2))	
	*/

  	var textPredsRels= (textPreds ++ textRels);
  	var hypPredsRels= (hypPreds ++ hypRels);
  	lhsSentence = Sts.text
  	rhsSentence = Sts.hypothesis
  	this.ruleWeighter = ruleWeighter;
  	vectorspace = vecspaceFactory((textPreds  ++ hypPreds).map(exp => boxerExpsToString(exp, false)).toSet)
  	
  	var textAtoms = textPredsRels.map(boxerExpsToString(_, false))
  	var hypAtoms = hypPredsRels.map(boxerExpsToString(_, true))
  	val textAtomsRR = textAtoms.unzip._2.map(t => List((t._2))).toList
  	val hypAtomsRR = List(hypAtoms.unzip._2.map(t =>  "-" + t._2).toList)
	//println (textAtomsRR);
	//println (hypAtomsRR);
  	val result = Resolution.resolveToFindDifference(textAtomsRR, hypAtomsRR);
  	//println("TXT: " + textAtoms);
  	//println("HYP: " + hypAtoms);
  	if (result.isDefined)
  	{
  		var ir:InferenceRule = result.get._1;
  		var ruleLhsRhs = ir.getCleanedExtendedRule;
  		textAtomsMap = textAtoms.toMap
		hypAtomsMap = hypAtoms.toMap
 		
		//Try to match existentially quantified variables on the RHS with variables on the LHS
		ruleLhsRhs = (ruleLhsRhs._1, findApplyMatched(ruleLhsRhs._1, ruleLhsRhs._2, false));
  		ruleLhsRhs = (ruleLhsRhs._1, findApplyMatched(ruleLhsRhs._1, ruleLhsRhs._2, true));

		//doDepParse (ruleLhsRhs._1, ruleLhsRhs._2, lhsSentence, rhsSentence, textAtomsMap:Map[String, (BoxerExpression, String)], textAtomsMap:Map[String, (BoxerExpression, String)])
		/*
		val updatedLhsVars = ruleLhsRhs._1.flatMap(_.argList);
		val updatedRhsVars = ruleLhsRhs._2.flatMap(_.argList);
		
		val varsStatistics = "("+ updatedLhsVars.size + "," + (updatedLhsVars & updatedRhsVars).size + "," + (updatedLhsVars -- updatedRhsVars).size + "," + (updatedRhsVars -- updatedLhsVars).size  + 
									"," + (updatedLhsVars -- updatedRhsVars).size + "," + (updatedRhsVars -- updatedLhsVars).size + ")"
		
//		val varsStatistics = ""
		val lhsExps:Set[BoxerExpression] = ruleLhsRhs._1.map(l => literalToBoxerExp(l, textAtomsMap)).toSet
		val rhsExps:Set[BoxerExpression] = ruleLhsRhs._2.map(l => literalToBoxerExp(l, hypAtomsMap)).toSet
		val pattern = Rules.sortVarsRenameVarsGetPattern(lhsExps)._2  + "--" + Rules.sortVarsRenameVarsGetPattern(rhsExps)._2 ;
		val lhs = PhrasalRules.ruleSideToString(lhsExps.toList, lhsSentence, false);
		val rhs = PhrasalRules.ruleSideToString(rhsExps.toList, rhsSentence, false);
		
		println ("[X"+pattern+"]\t" + "\t" + varsStatistics + "\t"+ lhs +"\t"+rhs +"\t" + ruleLhsRhs._1.mkString(",") + "\t" + ruleLhsRhs._2.mkString(",")+ "\t" + Sts.goldStandard  + "\t" + Sts.pairIndex)
		
  		*/

  		var (lhsConnectedSets, rhsConnectedSets)  = getConnectedSets(ruleLhsRhs);
		var rules :  List[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] = List();
		var matchedSets:Set[(Set[Literal], Set[Literal])] = Set();
		var unmatchedRhsLiterals:Set[Literal] = Set();
		var unmatchedLhsLiterals:Set[Literal] = Set();
		
  		rhsConnectedSets.foreach(rhsSet => 
  		{
  			val correspondingLhsSet = lhsConnectedSets.filter( lhsSet => (lhsSet._1 & rhsSet._1).size > 0);
  			assert(correspondingLhsSet.size <= 1);
  			if (correspondingLhsSet.size == 1)
  			{
  				if (Sts.opts.splitDiffRules)
  				{
	  				val rhsSplits = verbBasedSplitting(rhsSet._2)
	  				val lhsSplits = verbBasedSplitting(correspondingLhsSet.head._2)
	  				rhsSplits.foreach( rhsSplit => {
	  					lhsSplits.foreach( lhsSplit => {
	  						if ((lhsSplit._1 & rhsSplit._1).size > 0) //variable intersection
	  							matchedSets = matchedSets + ((lhsSplit._2, rhsSplit._2))
	  					})
	  				})
  				}
  				else
  					matchedSets = matchedSets + ((correspondingLhsSet.head._2, rhsSet._2))

  				lhsConnectedSets = lhsConnectedSets -- correspondingLhsSet; 
  			}
  			else unmatchedRhsLiterals = unmatchedRhsLiterals ++ rhsSet._2 
  		})
  		
  		unmatchedLhsLiterals = lhsConnectedSets.flatMap(_._2)
  		if (unmatchedRhsLiterals.size != 0) //if there are still some unmatched stuff in RHS
  		{
  			if (unmatchedLhsLiterals.size == 0) // if all of LHS was matched
  			{
  				if (matchedSets.size > 0  && Sts.opts.extendDiffRulesLvl == 2 /*full extend*/)  //do this ugly trick ONLY if I am extending rules. 
  					unmatchedLhsLiterals = matchedSets.head._1 //ugly. 
  			}
  			matchedSets = matchedSets + ((unmatchedLhsLiterals, unmatchedRhsLiterals))
  		}

  		
  		matchedSets.foreach (pair => {
  			var matchedRhs = findApplyMatched(pair._1, pair._2, false);
  			matchedRhs = findApplyMatched(pair._1, matchedRhs, true);
  			val rule = addRule(pair._1, matchedRhs, matchedSets.size);
  			if (rule.isDefined)
  				rules = rules :+ rule.get
  		})
  		/*
  		lhsConnectedSets.foreach(lhsSet => 
  		{
  			if ((lhsSet._1 & rhsSet._1).size > 0)
  			{
  				matched = true;
  				//Try again (after partitioning) to match existentially quantified variables on the RHS with variables on the LHS
  				var matchedRhs = findApplyMatched(lhsSet._2, rhsSet._2, false);
  				matchedRhs = findApplyMatched(lhsSet._2, matchedRhs, true);
  				rules = rules :+ addRule(lhsSet._2, matchedRhs).get;
  			}
  		})
  		if (!matched)
  		{
  			addRule(Set(), rhsSet._2);
  		}

  		*/
  		
		return if (Sts.opts.diffRules)rules; else List(); //return rules, or just print them
  	}
  	return List()
  }

	def addRule (lhsSet:Set[Literal], rhsSet:Set[Literal], rulesCountPerPair:Int) : Option[(BoxerDrs, BoxerDrs, Double, RuleType.Value)]= 
	{
		val ruleEvaluation = evaluateRule(lhsSet, rhsSet);
		val ruleEvalStr = ruleEvaluation._1 + "," + ruleEvaluation._2 + "," + ruleEvaluation._3;
		
		val lhsExps:Set[BoxerExpression] = lhsSet.map(l => literalToBoxerExp(l, textAtomsMap)).toSet
		val rhsExps:Set[BoxerExpression] = rhsSet.map(l => literalToBoxerExp(l, hypAtomsMap)).toSet
		var lhsDrs = Rules.boxerAtomsToBoxerDrs(lhsExps);
		var rhsDrs = Rules.boxerAtomsToBoxerDrs(rhsExps);
		//for pattern
//		val lhsExpsPattern:Set[BoxerExpression] = lhsSet.filter(_.predSymbol != "group_head-n").map(l => literalToBoxerExp(l, textAtomsMap)).toSet
//		val rhsExpsPattern:Set[BoxerExpression] = rhsSet.filter(_.predSymbol != "group_head-n").map(l => literalToBoxerExp(l, hypAtomsMap)).toSet
//		var pattern = Rules.sortVarsRenameVarsGetPattern(lhsExpsPattern)._2  + "--" + Rules.sortVarsRenameVarsGetPattern(rhsExpsPattern)._2 ;
		var pattern = Rules.sortVarsRenameVarsGetPattern(lhsExps)._2  + "--" + Rules.sortVarsRenameVarsGetPattern(rhsExps)._2 ;

		val lhsVarsToIndexMap:Map[String, String] = lhsSet.flatMap(l => {  // a map from variable to word index.
			if (l.argList.size == 1 /*&& (l.predSymbol.endsWith("n") ||l.predSymbol.endsWith("v"))*/)
			{
				val e = literalToBoxerExp(l, textAtomsMap)
				val idx = e match
				{
					case BoxerPred(discId, indices, variable, name, pos, sense) => 
						Rules.indicesToIndex(Rules.indicesToOneIndex(indices));
					case BoxerRel(discId, indices, event, variable, name, sense) => 
						Rules.indicesToIndex(Rules.indicesToOneIndex(indices));
				}
				Some((l.argList.head -> idx))
			}
			else 
				None
		}).groupBy(_._1).map { case (k, v) => (k -> v.unzip._2.toList.sortWith((x, y) => x<y).mkString(","))}

		var lhs = PhrasalRules.ruleSideToString(lhsExps.toList, lhsSentence, false, null);
		var rhs = PhrasalRules.ruleSideToString(rhsExps.toList, rhsSentence, false, lhsVarsToIndexMap);

		/// --check if they are already in wordnet if lexical, and phrasal if not lexical
		var isInWordnet = "Phrasal";
		if (lhsExps.size == 1 && rhsExps.size == 1 && lhsExps.head.isInstanceOf[BoxerPred] && rhsExps.head.isInstanceOf[BoxerPred])
		{
			isInWordnet = "NotInWN";
			val lhsPred = lhsExps.head.asInstanceOf[BoxerPred];
			val rhsPred = rhsExps.head.asInstanceOf[BoxerPred];
			val lhsPredName = /*lhsPred.name*/Lemmatize.lemmatizeWords(lhsPred.name)
			val rhsPredName = /*rhsPred.name*/Lemmatize.lemmatizeWords(rhsPred.name)
			//println ("CheckWordnetWith: " + lhsPredName + ", " + rhsPredName)
			val synonyms = WordNetRules.wordnet.getSynonyms(lhsPredName, lhsPred.pos);
			val hypernyms = WordNetRules.wordnet.getHypernyms(lhsPredName, lhsPred.pos);
			val hyponyms = WordNetRules.wordnet.getHyponyms(lhsPredName, lhsPred.pos);
			val antonyms = WordNetRules.wordnet.getAntonyms(lhsPredName, lhsPred.pos);
			if (rhsPred.pos == lhsPred.pos && rhsPredName != lhsPredName)
			{
				if (synonyms.contains(rhsPredName))
					isInWordnet = "Synonym"
				else if (hypernyms.contains(rhsPredName))
					isInWordnet = "Hypernym"
				else if(hyponyms.contains(rhsPredName))   //backward implication 
					isInWordnet = "Hyponym" 
				else if (antonyms.contains(rhsPredName))
					isInWordnet = "Antonym"
			}
		}
		  /// end wordnet check

		var gs:Double = Sts.goldStandard match {
			  case 0.5 => 0;
			  case 1 => 1;
			  case 0.0 => 1;
			  case _ => 0
		}
		var ruleType = RuleType.Implication;
		if (!GraphRules.isTextNegated && !GraphRules.isHypNegated  ) //(not negated, not negated) in Phase.hGivenT
		{
			if (Sts.goldStandard == 0) //contradiction
			{
				val splits = pattern.split("--");
				//if (splits.head.size == 1 && splits.last.size == 1)
				//{
				//Always set weight to -1 even if wordnet and distribution semantics can not get it
					gs = -1
					ruleType = RuleType.Opposite;
				//}
			}
		}
		val lhsVars = lhsSet.flatMap(_.argList);
		val rhsVars = rhsSet.flatMap(_.argList.filter(_.charAt(0).isUpper));
		val varsStatistics = "("+ lhsVars.size + "," + (lhsVars & rhsVars).size + "," + (lhsVars -- rhsVars).size + "," + (rhsVars -- lhsVars).size + ")"
		if (lhsSet.size == 0 )
		{
	  		//no rectangular brackets printed because I do not want these rules in the output file
			GraphRules.printDiffRules ("UNMATCHED\t" +pattern+"\t" + varsStatistics + "\t"+ rhsSet.mkString(",") + "\t" + rhsSet.mkString(",")+ "\t" + gs + "\t" + Sts.pairIndex )
			return None;
		}
		else
		{
			/*if ((rhsVars -- lhsVars).size == 0 && lhs != "" && rhs != "")
			{
				pattern = pattern.replace("r", "");
				val splits = pattern.split("--");
				if (splits.size == 2 && splits(0).length()<= 3 && splits(1).length()<= 3)
				println ("["+pattern+"]\t" + lhs + "\t" + rhs+ "\t" + gs + "\t"+ gs + "\t" + isInWordnet +"\t" +Sts.text + "\t" + Sts.hypothesis + "\t" + lhsDrs + "\t" + rhsDrs)
			}*/

			var dropRule = false
			var simpleLhsText = PhrasalRules.ruleSideToString(lhsExps.toList, lhsSentence, true, null);
			var simpleRhsText = PhrasalRules.ruleSideToString(rhsExps.toList, rhsSentence, true, null);
			
			simpleLhsText = handleTransparentNouns(simpleLhsText)
			simpleRhsText = handleTransparentNouns(simpleRhsText)
			lhs = handleTransparentNouns(lhs)
			rhs = handleTransparentNouns(rhs)

			var notSure:Int = if (gs != 1 /*generate it for Neutral and Contra*/&& rulesCountPerPair != 1) 1//rulesCountPerPair 
							  else
							    0


			if (notSure > 0 && GraphRules.sureRules.contains(simpleLhsText + "\t" + simpleRhsText))
			{
				gs = 1;
			} 

			if (simpleLhsText.contains("nobody") || simpleRhsText.contains("nobody")) //ignore rules with "nobody". Wordnet and Distributional semantics have no hope of getting them right. I hardcoded them
			{
				pattern = "NOBODY: " + pattern;
				dropRule = true
			}
			else if (lhs == "" || rhs == "") //do not print rectangular brackets becaus I do not need empty rules to be printed
			{
				pattern = "EMPTY: " + pattern;
				dropRule = true
			}
			else 
				pattern = "["+pattern+"]";


			var ruleString = ""

			if (Sts.opts.diffRulesSimpleText)
				ruleString = pattern + "\t" + varsStatistics + "\t" + Sts.pairIndex + "\t"+ simpleLhsText + "\t" + simpleRhsText+ "\t#" + gs +"#\t" +  notSure /*: not sure is always zero, no, I need it back*/  + "\t" + isInWordnet + "\t" +  ruleEvalStr + "\t" + lhsSet.mkString(",") + "\t" + rhsSet.mkString(",")+ "\t" + Sts.opts.extendDiffRulesLvl.get 
			else 
				ruleString = pattern + "\t" + GraphRules.ruleID + "\t"+lhs + "\t" + rhs+ "\t#" + gs + "#\t#"+ gs +"#\t" +  notSure /*: not sure is always zero, no, I need it back*/ + "\t" + isInWordnet + "\t" + ruleEvalStr + "\t" + Sts.opts.extendDiffRulesLvl.get + "\t" + Sts.pairIndex +"\t"+ lhsSentence + "\t" + rhsSentence + "\t" + lhsDrs + "\t" + rhsDrs
			GraphRules.ruleID = GraphRules.ruleID  + 1
			
			GraphRules.printDiffRules(ruleString);
	
			var key = "#" + (if(gs == -1.0) 0.0 else gs)   + "#" + notSure + "#" + simpleLhsText + "#" + simpleRhsText;

/*
			if (gs != 1 && notSure > 0) //if rule is a notSure
			{
				//search for a true rule
				val searchFor = "#" + "1.0" + "#" + "0" + "#" + simpleLhsText + "#" + simpleRhsText;
				//if not found
				if (!GraphRules.allGeneratedRules.unzip._1.toList.contains(searchFor))
					None //do nothing
				else
				{
					println ("SURE: " + ruleString)
					//replace the zero gold standard weight of the current rule to 1
					ruleString = ruleString.replaceAll("#0.0#", "#1.0#")
					key = key.replaceAll("#0.0#", "#1.0#")
				}
			}
			else if (gs == 1)  //if a true rule
			{
				//search for notSure
				val searchFor = "#0.0" + "#" + "1" + "#" + simpleLhsText + "#" + simpleRhsText;
				//if found
				if (GraphRules.allGeneratedRules.unzip._1.toList.contains(searchFor))
					GraphRules.allGeneratedRules = GraphRules.allGeneratedRules.map( line => {
						if (line._1 == searchFor)
						{
							println ("SURE: " + line._2)
							(line._1.replaceAll("#0.0#", "#1.0#"), line._2.replaceAll("#0.0#", "#1.0#"))
						}
						else line
					})
					

			}
*/

			if (!dropRule)  
				GraphRules.allGeneratedRules  = GraphRules.allGeneratedRules :+ (key, ruleString)
				
			//if (simpleLhsText == simpleRhsText) //I DO NOT NEED THIS CASE ANYMORE. I already get it from Stephen's rules
				///*OLD*/return  Some((((lhsDrs, rhsDrs, /*rw.head._2.get*/ if(gs != 0) Double.PositiveInfinity else 0.6, ruleType))))
			//	return  Some((((lhsDrs, rhsDrs,  Double.PositiveInfinity, RuleType.Implication))))
			if	(!dropRule && 
					(
						/*(simpleLhsText.contains(simpleRhsText) && transparentNouns.contains( simpleLhsText.replaceFirst(simpleRhsText, "").trim ))
						||(simpleRhsText.contains(simpleLhsText) && transparentNouns.contains( simpleRhsText.replaceFirst(simpleLhsText, "").trim ))
						||*/
						//	isInWordnet == "Hypernym"
							(ruleEvaluation._1 == 0 && ruleEvaluation._2  == 0) //all words are matched
						//|| isInWordnet == "Synonym"
						//|| isInWordnet == "Antonym"
						|| simpleLhsText == simpleRhsText
						|| (GraphRules.trueRules.contains(simpleLhsText + "\t" + simpleRhsText) /*&& isInWordnet ==  "Phrasal"*/ )
					)
				)
				return  Some((((lhsDrs, rhsDrs,  Double.PositiveInfinity, RuleType.Implication))))
			else
				None
		}
	}
	
  val transparentNouns:List[String] = List("group of", "slice of", "piece of", "can of" )
  val transparentNounsRegx1 = transparentNouns.map(_+" ").mkString("|").r
  val transparentNounsRegx2 = transparentNouns.map( tn => tn.split(" ").map(w => w + "-[n,a,v,r]-[0-9]+ ").mkString("") ).mkString("|").r
  
  def handleTransparentNouns (ruleSide : String) : String = 
  {
    var txt = ruleSide;
    txt = transparentNounsRegx1.replaceAllIn(txt, "")
    txt = transparentNounsRegx2.replaceAllIn(txt, "")
    txt
  }

def evaluateRule (inputRuleLhs:Set[Literal], inputRuleRhs:Set[Literal]): (Int, Int, Int) = 
  {
	var ruleLhs = inputRuleLhs;
	var ruleRhs = inputRuleRhs;
	val lhsVarsEntities = ruleLhs.flatMap(l => {  // a map from variable to its most representative content word
		if (l.argList.size == 1)
			Some((l.argList.head -> l.predSymbol))
		else 
			None
	}).groupBy(_._1).map {case (k, v) => (k, v.unzip._2)}
	
	
	var unAligned: Int = 0;
	var unMatched: Int = 0;
	var matched: Int = 0;
	ruleRhs.foreach(l => 
	{
		if (l.argList.size == 1)
		{
			val rhsWordPos = l.predSymbol
			val rhsW = rhsWordPos.substring(0, rhsWordPos.length() - 2);
			val rhsPos = rhsWordPos.charAt(rhsWordPos.length()-1)+"";
			
			val aligned = lhsVarsEntities.get(l.argList.head);
			if (aligned.isEmpty)
				unAligned = unAligned + 1;
			else
			{
				var matchFound:Boolean = false;
				aligned.get.foreach( lhsWordPos => {
					val lhsW = lhsWordPos.substring(0, lhsWordPos.length() - 2);
					val lhsPos = lhsWordPos.charAt(lhsWordPos.length()-1)+"";
					
					val synonyms = WordNetRules.wordnet.getSynonyms(lhsW, lhsPos) ++ WordNetRules.wordnet.getSynonyms(Lemmatize.lemmatizeWord(lhsW), lhsPos) 
					val hypernyms = WordNetRules.wordnet.getHypernyms(lhsW, lhsPos) ++ WordNetRules.wordnet.getHypernyms(Lemmatize.lemmatizeWord(lhsW), lhsPos);
					
					if (lhsW == rhsW || Lemmatize.lemmatizeWord(lhsW) == Lemmatize.lemmatizeWord(rhsW) || (synonyms ++ hypernyms).contains(rhsW))
						matchFound = true;
				})
				if (matchFound)
					matched = matched + 1;
				else 
					unMatched = unMatched + 1;
			}
		}
	})
	
	return (unAligned, unMatched, matched);
  }
  
  def findApplyMatched (inputRuleLhs:Set[Literal], inputRuleRhs:Set[Literal], matchAlreadyMatched: Boolean): Set[Literal] = 
  {
	var ruleLhs = inputRuleLhs;
	var ruleRhs = inputRuleRhs;
	val lhsVars = ruleLhs.flatMap(_.argList);
	val rhsVars = ruleRhs.flatMap(_.argList);
	val lhsVarsEntities = ruleLhs.flatMap(l => {  // a map from variable to its most representative content word
		if (l.argList.size == 1 && (l.predSymbol.endsWith("n") ||l.predSymbol.endsWith("v")))
			Some((l.argList.head -> l.predSymbol))
		else 
			None
	}).toMap
	val rhsVarsEntities = ruleRhs.flatMap(l => { // a map from variable to its most representative content word
		if (l.argList.size == 1 && (l.predSymbol.endsWith("n") ||l.predSymbol.endsWith("v")))
			Some((l.argList.head -> l.predSymbol))
		else 
			None
	}).toMap

	var lhsNotMatchedVars = if (matchAlreadyMatched) lhsVars  else (lhsVars -- rhsVars)
	var rhsNotMatchedVars = (rhsVars -- lhsVars).filter(_.charAt(0).isUpper) // a list of existentially quantified variables only

	def applyMatch (lhsVarToMatch: String, rhsVarToMatch: String) = 
	{
		ruleRhs = ruleRhs.map(l => {
			 val newArgList = l.argList.map(arg => {
				 if (arg == rhsVarToMatch)
					 lhsVarToMatch;
				 else
					 arg;
			 })
			 l.argList = newArgList;
			 l;
		 })
	}

	//DO NOT USE THIS TRICK ANYMORE. IT WAS JUST FOR TESTING
	//if (lhsVars.size > 0)
	//   rhsNotMatchedVars.map (rhsV => applyMatch (/*"xir"*/ lhsVars.head, rhsV))


	///////**********************************************************
	//hard coded variable matching rules for trivial but common cases.	
	//////***********************************************************
	//1)lhs unmatched vars = 0, rhs unmatched vars = 1
	//example: blond woman => woman with blond hair
	//matching rule: introduce new constant in rhs
	if (lhsNotMatchedVars.size == 0 && rhsNotMatchedVars.size == 1)
		rhsNotMatchedVars.map (rhsV =>
		{
			applyMatch ("xir", rhsV)
			GraphRules.printDiffRules("MATCH APPLIED(rule1): " + "XIR" + "=>" + rhsVarsEntities.getOrElse(rhsV, rhsV) + "\t" + Sts.goldStandard + "\t" +  Sts.pairIndex )
			rhsNotMatchedVars = rhsNotMatchedVars - rhsV;
		})
	//2)lhs unmatched vars = 1, rhs unmatched vars = 1
	//example: man => person 
	//matching rule: trivial
	//This is usually correct, but fails sometimes.
	if (lhsNotMatchedVars.size == 1 && rhsNotMatchedVars.size == 1)
		rhsNotMatchedVars.map (rhsV => 
		{
			applyMatch (lhsNotMatchedVars.head, rhsV)
			GraphRules.printDiffRules("MATCH APPLIED(rule2): " + lhsVarsEntities.getOrElse(lhsNotMatchedVars.head, lhsNotMatchedVars.head) + "=>" + rhsVarsEntities.getOrElse(rhsV, rhsV) + "\t" + + Sts.goldStandard+ "\t" +  Sts.pairIndex )
			rhsNotMatchedVars = rhsNotMatchedVars - rhsV;
		})
	///////***********************END hard coded matching rules.
		
	var changed = true;
	var unique = true;
	while (changed) 
	{
		changed = false
		unique = true
		var lhsMatchedVar = "";
		var rhsMatchedVar = "";
		try
		{
			rhsNotMatchedVars.foreach(rhsV => {
				val rhsWordPosOption = rhsVarsEntities.get(rhsV)
				if (rhsWordPosOption.isEmpty)
				{
					//remove
					rhsNotMatchedVars = rhsNotMatchedVars - rhsV;
					applyMatch("xir", rhsV) //replace it with a Constant
					GraphRules.printDiffRules("MATCH APPLIED(rule3): " + "XIR" + "=>" + rhsV + "\t" + Sts.goldStandard + "\t" +  Sts.pairIndex )
					throw AllDone;
				}
				val rhsWordPos = rhsWordPosOption.get
				val rhsWord = rhsWordPos.substring(0, rhsWordPos.length() - 2);
				val rhsPos = rhsWordPos.charAt(rhsWordPos.length()-1)+"";
	
				try
				{
					lhsNotMatchedVars.foreach(lhsV => {
						val lhsWordPosOption = lhsVarsEntities.get(lhsV)
						if (lhsWordPosOption.isEmpty)
						{
							//remove
							lhsNotMatchedVars = lhsNotMatchedVars - lhsV;
							throw AllDone;
						}
						val lhsWordPos = lhsWordPosOption.get
						val lhsWord = lhsWordPos.substring(0, lhsWordPos.length() - 2);
						val lhsPos = lhsWordPos.charAt(lhsWordPos.length()-1)+"";
											
						//val rw = ruleWeighter.weightForRules(lhsWordPos+"-0", List(), Seq((rhsWordPos+"-0", List())).toMap, vectorspace);
						
						val synonyms = WordNetRules.wordnet.getSynonyms(lhsWord, lhsPos);
						val hypernyms = WordNetRules.wordnet.getHypernyms(lhsWord, lhsPos);

						//val hyponyms = WordNetRules.wordnet.getHyponyms(lhsWord, lhsPos);  //think about these when do contradiction
						//val antonyms = WordNetRules.wordnet.getAntonyms(rhsWord, lhsPred.pos); //think about these when do contradiction
						
						if (lhsWord == rhsWord || (synonyms ++ hypernyms).contains(rhsWord) || Lemmatize.lemmatizeWord(lhsWord) == Lemmatize.lemmatizeWord(rhsWord))
						{
							if (changed) //make sure it is only one possible match
							{
								GraphRules.printDiffRules("MORE THAN ONE MATCH: " + lhsWordPos +"=>"+rhsWordPos + "\t" + Sts.goldStandard + "\t" +  Sts.pairIndex )
								unique = false;
								lhsNotMatchedVars = lhsNotMatchedVars - lhsV;
								throw HalfDone;
							}
							else 
							{
								GraphRules.printDiffRules("MATCH FOUND: " + lhsWordPos +"=>"+rhsWordPos + "\t" + Sts.goldStandard + "\t" +  Sts.pairIndex )
								changed = true;
								lhsMatchedVar = lhsV;
								rhsMatchedVar = rhsV;
							}
						}
	
					})
				}catch {case HalfDone =>}
			})
			if (changed) //changed
			{
				lhsNotMatchedVars = lhsNotMatchedVars - lhsMatchedVar;
				rhsNotMatchedVars = rhsNotMatchedVars - rhsMatchedVar;
				if (unique) // and there is a unique match
				{
					applyMatch(lhsMatchedVar, rhsMatchedVar)
					GraphRules.printDiffRules("MATCH APPLIED(rule0): " + lhsVarsEntities.get(lhsMatchedVar) + "=>" + rhsVarsEntities.get(rhsMatchedVar) + "\t" + + Sts.goldStandard + "\t" +  Sts.pairIndex )
				}
			}
		}catch {case AllDone =>}
	}
	
	return ruleRhs
  }

  def literalToBoxerExp(l:Literal, termsMap: Map[String,(BoxerExpression, String)]): BoxerExpression = 
  {
  	val entry = termsMap.get(l.predSymbol);
  	assert (entry.isDefined);
  	entry.get._1 match 
  	{
  		case BoxerPred(discId, indices, variable, name, pos, sense) => if(l.argList.length != 1) println ("ERROR: a relation should not be replaced with a predicate"); return BoxerPred(discId, indices, BoxerVariable(l.argList(0)/*.toLowerCase*/), name, pos, sense)
		case BoxerRel(discId, indices, event, variable, name, sense) => if(l.argList.length != 2) {
			println ("ERROR: a predicate should not be replaced with a relation"); 
			//ugly hack
			return BoxerRel(discId, indices, BoxerVariable(l.argList(0)/*.toLowerCase*/), BoxerVariable(l.argList(0)/*.toLowerCase*/), name, sense)
		}
		else
			return BoxerRel(discId, indices, BoxerVariable(l.argList(0)/*.toLowerCase*/), BoxerVariable(l.argList(1)/*.toLowerCase*/), name, sense)
		case _ => throw new RuntimeException("Unexpected BoxerExpression: " + l);
	}
  	throw new RuntimeException("Unreachable");
  }
  
  def boxerExpsToString(s:BoxerExpression, uppecaseVar:Boolean) : (String, (BoxerExpression, String)) = 
  {
  	s match 
  	{
  		case BoxerPred(discId, indices, variable, name, pos, sense) => (name + "-" + pos, (s, name + "-" + pos + "(" + (if (uppecaseVar) variable.name.toUpperCase() else variable.name )+ ")"))
		case BoxerRel(discId, indices, event, variable, name, sense) => (name + "-r" , (s, name + "-r(" + (if (uppecaseVar) event.name.toUpperCase() else event.name ) + "," + (if (uppecaseVar) variable.name.toUpperCase() else variable.name ) +")" ));
		case _ => throw new RuntimeException("Unexpected BoxerExpression: " + s);
	}
  }
  
  //a set of connected sets
  //each connected set is a pair 
  //each pair is: 
  //1) a set of variables in the set
  //2) a set of literals in the set
  def getConnectedSets (literals: (Set[Literal], Set[Literal]) ): (Set[(Set[String], Set[Literal])], Set[(Set[String], Set[Literal])]) =  
  {        	
    // each group of connected sets is: 
  	//1) each literal is a separate group (just for now, they will be merged later on)
  	//2) variables from the other set. This is important for cases where for example the lhs is disconnected but its corresponding rhs is connected. 
  	var lhsConnectedSets:Set[(Set[String], Set[Literal])] = literals._1.map(l => (l.argList.toSet, Set(l))).toSet ++ literals._2.map(l => (l.argList.toSet, Set[Literal]())).toSet
  	var rhsConnectedSets:Set[(Set[String], Set[Literal])] = literals._2.map(l => (l.argList.toSet, Set(l))).toSet ++ literals._1.map(l => (l.argList.toSet, Set[Literal]())).toSet
  	//val relations:Set[Set[String]] = (literals._1 ++ literals._2).flatMap(l => if (l.argList.size > 1) Some(l.argList.toSet) else None )
  	
  	lhsConnectedSets = groupEqvSets(lhsConnectedSets).filter(_._2.size > 0)
  	rhsConnectedSets = groupEqvSets(rhsConnectedSets).filter(_._2.size > 0)
  	
    return (lhsConnectedSets, rhsConnectedSets);
  }
  def groupEqvSets (inputSets: Set[(Set[String], Set[Literal])]): Set[(Set[String], Set[Literal])] =  
  {
    var connectedSets = inputSets
    var relations: Map[Set[String],Set[Literal]] = Map();
    if (Sts.opts.splitDiffRules)
    {
		//literals with single variable
	    connectedSets = inputSets.filter(_._1.size == 1); 
	  	//if two relations have the same set of variables, group them in one entry  
	  	relations = inputSets.filter(_._1.size == 2).groupBy(_._1).map{case (k,v) => (k,v.flatMap(_._2))}
    }
  	
    var changed = true;
    while (changed) {
      changed = false
      try{
          connectedSets.foreach(outerSet =>
          {
              connectedSets.foreach(innerSet =>
                {
                  if (outerSet != innerSet) //not the same entry
                  {
                    val connectingRelationsVariables = relations.keys.filter(k => ((outerSet._1 ++ innerSet._1) & k ).size == k.size);
                    val connectingRelations = connectingRelationsVariables.map(relations)
                    if ((outerSet._1 & innerSet._1).size != 0 || connectingRelations.size > 0) 
                    { //if there is an intersection 
                      val newEqvSet = (  ((outerSet._1 ++ innerSet._1), (outerSet._2 ++ innerSet._2 ++ connectingRelations.flatten ))  )  //group them in one set 
                      connectedSets = connectedSets.filter(e=> (e != outerSet && e != innerSet)); //remove the two small sets
                      connectedSets  = connectedSets  + newEqvSet; //and add this set to the connectedSets
                      relations = relations -- connectingRelationsVariables
                      changed = true;
                      throw AllDone;  //simulating "break". This is important because changing groupedEq messes up the outer two loops. 
                    }
                  }
                })
            })
        }catch {case AllDone =>}
    }
    
    //remaining relations should be added somewhere
    relations.foreach(rel => {
    	val sets = connectedSets.filter(set => (set._1 & rel._1).size > 0).toSet //.toSet to remove duplicates
    	//any relation that is not connected to content words will be dropped
    	//that will result into useless rules in some cases, but that is ok 
    	//that is better than creating wrong rules
    	//The hope is that with higher extension level, good rules will be constructed.
    	if (sets.size > 0)
    	{
    		if (sets.size > 1)
    		  LOG.error("After finding connected sets, error with connecting remaining relations: " + rel + "->" + sets)
    		val newEqvSet = ((sets.head._1), (sets.head._2  ++ rel._2))  //add the relation to the literals, but do not add it to the variables  
    		connectedSets = connectedSets - sets.head;  //remove the old set
    		connectedSets  = connectedSets  + newEqvSet; //add the new set
    	}
    })
    return connectedSets;
  }
  
  
  def verbBasedSplitting(literals: Set[Literal]): Set[(Set[String], Set[Literal])] = 
  {
	  val verbsSets = literals.filter(l => l.predSymbol.endsWith("-v") && l.argList.size == 1).groupBy(l => l.predSymbol).map{case (k,v) => {
		  (v.map(_.argList.head), v) 
	  }}.toSet

	  //If there are no verbs in the literals, return the literals as they are with no change. 
	  if (verbsSets.size == 0)
	    return Set((literals.flatMap(_.argList), literals))

	  var splitOnVerbs:Set[(Set[String], Set[Literal])] = verbsSets.map(verb => 	  
	  {
		  var collectedVars:Set[String] = verb._1
		  var collectedLiterals:Set[Literal] = verb._2		  
		  //so stuff
		  var change = true;
		  while (change)
		  {
			  change = false;
			  val matchedLiterals = literals.filter( l => {
				  (l.argList.toSet & collectedVars).size > 0 /*using variables in the selected set*/ && 
				  (l.predSymbol != "agent-r"  /*it is not an agent relation */ 
				   		|| (l.predSymbol == "agent-r"  && l.argList.size == 2 && verb._1.contains(l.argList(0)) )) && /*it is the agent connected the this particular verb*/
				  !l.predSymbol.endsWith("-v") /*do not add any more verbs*/
			  })
			  if ((matchedLiterals -- collectedLiterals).size > 0)
			  {
				  change = true
				  collectedLiterals = collectedLiterals ++ matchedLiterals;
				  collectedVars = collectedVars ++ matchedLiterals.flatMap(_.argList) -- verbsSets.flatMap(_._1) ++ verb._1
			  }
		  }
		  
		  ((collectedVars, collectedLiterals))
	  })
	  val unusedLiterals = literals -- splitOnVerbs.flatMap(_._2)
	  val unusedLiteralsAddedToHead = (splitOnVerbs.head._1 ++ unusedLiterals.flatMap(_.argList), splitOnVerbs.head._2 ++ unusedLiterals);
	  splitOnVerbs = splitOnVerbs - splitOnVerbs.head + unusedLiteralsAddedToHead
	  return splitOnVerbs;
  }
  def hasNegation(e: BoxerExpression): Boolean = {
  	//TODO: if has a predicate "nobody"
      e match {
      	case BoxerNot(discId, indices, drs) => {
      		drs match {
      			case BoxerDrs(refs, conds) => {
      				if (conds.size == 1 && conds.head.isInstanceOf[BoxerEq])
      					return false
      			}
      		}
      		return true;	
      	}
        case _ => e.visit(hasNegation, (x: List[Boolean]) => x.reduce(_ | _) , false)
      }
  }
  
  def doDepParse (lhsLiterals : Set[Literal], rhsLiterals: Set[Literal], text:String, hyp: String, textAtomsMap:Map[String, (BoxerExpression, String)], hypAtomsMap:Map[String, (BoxerExpression, String)]) = 
  {
/*
  	val textDepGraph = Sts.depParser.apply(text.split(" "));

  	if(textDepGraph.isDefined)
  	{
  		println(textDepGraph.get.graphviz)
  		println(lhsLiterals)
  		println(text)
  	}
  	val hypDepGraph = Sts.depParser.apply(hyp.split(" "));
  	if(hypDepGraph.isDefined)
  	{
  		println(hypDepGraph.get.graphviz)
  		println(rhsLiterals)
  		println(hyp)
  	}
*/  	
  	
  }
}

