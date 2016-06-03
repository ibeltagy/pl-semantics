package utcompling.mlnsemantics.run

import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.mlnsemantics.vecspace._
import org.apache.log4j.Logger
import org.apache.log4j.Level
import utcompling.mlnsemantics.inference._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.inference.support.WeightedExpression
import scala.collection.mutable.ListBuffer
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.mlnsemantics.rules.RuleType
import scalax.collection.edge.WDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.mutable.Graph
import scalax.collection.edge.LUnDiEdge
import scalax.collection.edge.LkUnDiEdge
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import scala.io.Source
import de.bwaldvogel.liblinear.SolverType
import de.bwaldvogel.liblinear.Parameter
import de.bwaldvogel.liblinear.Model
import de.bwaldvogel.liblinear.Linear
import java.io.File
import de.bwaldvogel.liblinear.Feature
import de.bwaldvogel.liblinear.FeatureNode
import weka.classifiers.functions.LibSVM
import libsvm.svm_problem
import libsvm.svm_node
import libsvm.svm_parameter
import libsvm.svm
import libsvm.svm_model
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import scala.collection.mutable.Queue

object Baseline
{
	def innerFeaturizer(featureString: Array[String], featureIndexShift:Int): List[Feature/*svm_node*/] =   //SVM
	{
		val f:List[Feature/*svm_node*/] = featureString  //SVM
								.toSet[String]
								.map(relDir => 
								{
									val relDirSplits = relDir.split("\\$");
									var directionShift = 0;
									if (relDirSplits.length == 1)
									{
										assert(relDirSplits(0) == "null");
									}
									else
									{
										assert(relDirSplits.length == 2)
										val dir = relDirSplits(1)
										assert (List("l2r", "r2l").contains(dir));
										if (dir == "r2l")
											directionShift = relNames.length
									}
									val rel = relDirSplits(0)
									assert(relNames.contains(rel), rel)
									val id = relNames.indexOf(rel)

									val n = new svm_node ();
									n.index = id + featureIndexShift + directionShift;
									n.value = 1.0;
									n;
									new FeatureNode(n.index, n.value)
								})
								.toList
								.sortBy(_.index)  //SVM
		return f;
	}

	def featurize(lhsRels:Array[String], rhsRels:Array[String]):Array[Feature/*svm_node*/] =
	{
		val conj = lhsRels.toSet.intersect(rhsRels.toSet).toArray;
		val diff1 = lhsRels.toSet.diff(rhsRels.toSet).toArray;
		val diff2 = rhsRels.toSet.diff(lhsRels.toSet).toArray;
		val f:Array[Feature/*svm_node*/] = (innerFeaturizer(lhsRels, 1) 
							++ innerFeaturizer(rhsRels, 1 + 2*relNames.length) 
							++ innerFeaturizer(conj, 1 + 4*relNames.length)
							++ innerFeaturizer(diff1, 1 + 6*relNames.length)
							++ innerFeaturizer(diff2, 1 + 8*relNames.length)
							).toArray
		f
	}

			
	def main(args: Array[String]) 
	{
		println(args.mkString(", "))
		Logger.getRootLogger.setLevel(Level.INFO)
		assert(args.length == 3)
		val trainOrTest = args(0);
		assert (List("train", "test").contains(trainOrTest));
		val modelPath = args(1); //save a model or read a saved model
		val inputFile = args(2); //could be training or testing file
		
		val features:scala.collection.mutable.ListBuffer[Array[Feature/*svm_node*/]] = ListBuffer[Array[Feature/*svm_node*/]]()
		val labels:scala.collection.mutable.ListBuffer[Double] = ListBuffer()
		val rand = scala.util.Random
		for (line <- Source.fromFile(inputFile).getLines) 
		{
			val splits = line.split (" - ");
			assert (splits.length == 4);
			assert (List("neg", "pos").contains(splits(0)));
			var label = 0.0;
			if (splits(0) == "pos")
				label = 1.0;
		
			val f:Array[Feature/*svm_node*/] = featurize(splits(2).split(", "), splits(3).split(", "));
			
			
			if (/*true*/label == 1.0 || rand.nextInt(8) == 0/*I keep only 1/7 of the negative training examples*/) //negative example
			{
				println(line)
				println (f.map(x => x.getIndex() + "-" + x.getValue()).mkString(", "))
				//println (f.map(x => x.index + "-" + x.value).mkString(", "))
				features.append(f)
				labels.+=(label);
			}
		}
		assert(features.length == labels.length)
		println (features.length + " -- " + labels.length)
		
		val problem = new de.bwaldvogel.liblinear.Problem();
		problem.l = features.length; //number of data points
		problem.n = relNames.length * 10 + 1; //number of features
		problem.x = features.toArray //features
		problem.y = labels.toArray // target values
		
		val solver:SolverType = SolverType.L1R_LR// -s 0
		val C:Double = 1.0;    // cost of constraints violation
		val eps:Double = 0.0001; // stopping criteria
		
		val parameter:Parameter = new Parameter(solver, C, eps);
		//parameter.setWeights(Array(6.3), Array(1))
		var model:Model = Linear.train(problem, parameter);
		val modelFile:File = new File(modelPath);
		model.save(modelFile);
		// load model or use it directly
		model = Model.load(modelFile);
		/*
		val problem = new svm_problem();
		problem.l = features.length; //number of data points
		//problem.n = relNames.length * 5 + 1; //number of features
		problem.x = features.toArray //features
		problem.y = labels.toArray // target values
		
		val solver:SolverType = SolverType.L2R_L2LOSS_SVC// -s 0
		val C:Double = 1.0;    // cost of constraints violation
		val eps:Double = 0.0001; // stopping criteria
		
		//val parameter:svm_parameter = new svm_parameter(solver, C, eps);
		val parameter:svm_parameter = new svm_parameter();
		//parameter.kernel_type = 0; //linear
		
		//parameter.weight = Array(6.3);
		//parameter.weight_label = Array(1);
		//var model:Model = Linear.train(problem, parameter);
		var model:svm_model = svm.svm_train(problem, parameter);
		svm.svm_save_model(modelPath, model)
		// load model or use it directly
		model = svm.svm_load_model(modelPath)
		*/
		/////
		var correct = 0;
		var total = 0;
		var truePositive = 0
		var predictedPositive = 0;
		var actualPositive = 0;
		(problem.x zip problem.y).foreach( t => { 
			val prediction:Double = Linear.predict(model, t._1);
			//val prediction:Double = svm.svm_predict(model, t._1);
			if (prediction == t._2)
				correct = correct + 1;
			if (prediction == t._2 && prediction == 1.0)
				truePositive = truePositive + 1
			if (prediction == 1.0)
				predictedPositive = predictedPositive + 1
			if (t._2 == 1.0)
				actualPositive = actualPositive + 1
			total = total + 1;
			//println ("prediction: " + prediction);
		})
		println ("Accuracy: " + correct + " / " + total + " = " + correct*1.0/total)
		var p = truePositive*1.0/predictedPositive
		var r = truePositive*1.0/actualPositive
		println ("Precision: " + truePositive + " / " + predictedPositive + " = " + p)
		println ("Recall: " + truePositive + " / " + actualPositive + " = " + r)
		println ("F1: " + 2.0 * p * r / (p + r))
	}
	
	val relNames = List(
		"null",
		"acl", //clausal modifier of noun (adjectival clause)
		"advcl", //adverbial clause modifier
		"advmod", //adverbial modifier
		"amod", //adjectival modifier
		"appos", //appositional modifier
		"aux", //auxiliary
		"auxpass", //passive auxiliary
		"case", //case marking
		"cc", //coordinating conjunction
		"ccomp", //clausal complement
		"compound", //compound
		"conj", //conjunct
		"cop", //copula
		"csubj", //clausal subject
		"csubjpass", //clausal passive subject
		"dep", //unspecified dependency
		"det", //determiner
		"discourse", //discourse element
		"dislocated", //dislocated elements
		"dobj", //direct object
		"expl", //expletive
		"foreign", //foreign words
		"goeswith", //goes with”
		"iobj", //indirect object
		"list", //list
		"mark", //marker
		"mwe", //multi-word expression
		"name", //name
		"neg", //negation modifier
		"nmod", //nominal modifier
		"nsubj", //nominal subject
		"nsubjpass", //passive nominal subject
		"nummod", //numeric modifier
		"parataxis", //parataxis
		"punct", //punctuation
		"remnant", //remnant in ellipsis
		"reparandum", //overridden disfluency
		"root", //root
		"vocative", //vocative
		"xcomp", //open clausal complement
		"nmod:npmod",
		"nmod:poss",
		"nmod:tmod",
		"acl:relcl", 
		"det:predet",
		"compound:prt",
		"ref",
		"rel"
	)
	//for the use with Baseline class and GraphRules
	var textPreds: Iterable[BoxerPred] = null
	var textRels: Iterable[BoxerRel] = null
	var hypPreds: Iterable[BoxerPred] = null
	var hypRels: Iterable[BoxerRel] = null
	var textEntities:List[String] = null;
	var textEntitiesMap:Map[String,Set[BoxerPred]] = null;
	var hypEntities:List[String] = null;
	var hypEntitiesMap:Map[String,Set[BoxerPred]] = null;
	var entityPotentialMatchs:Map[String,List[String]] = null
	var hypGraph:scalax.collection.mutable.Graph[String, LUnDiEdge] = null;
	var textGraph:scalax.collection.mutable.Graph[String, LUnDiEdge] = null;
	var model:Model = null;
	/////
}
class Baseline (delegate: ProbabilisticTheoremProver[BoxerExpression])
	extends ProbabilisticTheoremProver[BoxerExpression] 
{
	private val LOG = LogFactory.getLog(classOf[Baseline])
	
	override def prove(
		constants: Map[String, Set[String]],
		declarations: Map[BoxerExpression, Seq[String]],
		evidence: List[BoxerExpression],
		assumptions: List[WeightedExpression[BoxerExpression]],
		goal: BoxerExpression): Seq[Double] = 
	{
		Baseline.textPreds = null
		Baseline.textRels = null
		Baseline.hypPreds = null
		Baseline.hypRels = null
		Baseline.textEntities = null
		Baseline.textEntitiesMap = null
		Baseline.hypEntities = null
		Baseline.hypEntitiesMap = null
		Baseline.entityPotentialMatchs = null
		Baseline.hypGraph = null
		Baseline.textGraph = null
		if (Sts.opts.baseline == "dep" || Sts.opts.graphRules > 0) //same data strcuctures will be used in GraphRules. 
		{
			initDS (assumptions.head.expression, goal);
		}
		if (Sts.opts.baseline == "dep")
		{
			if (Sts.opts.ruleClsModel.isDefined)
				Baseline.model = Model.load(new File(Sts.opts.ruleClsModel.get));

			var maxScore = 0.0
			var maxScoreEntity = "";
			val placeholderNode = Baseline.hypGraph.get( Baseline.hypPreds.filter ( _.name == "@placeholder" ).head.variable.name )
			var hypEntitiesSorted:List[Graph[String, LUnDiEdge]#NodeT] = null;
			if (Sts.opts.graphRules == 2)
				hypEntitiesSorted = topologicalSort(placeholderNode)
			
			Sts.qaEntities/*.filter ( e => e._2 != "" && e._1 != "@placeholder" )*/.foreach(ent=>
			{
				var entityScore = 0.0;
				if (Sts.opts.graphRules == 1)
				{
					entityScore = entityScoreLevel1(ent._1, placeholderNode);
				}
				else if (Sts.opts.graphRules == 2)
				{
					entityScore = entityScoreLevel2(ent._1, hypEntitiesSorted);
				}
				else throw new RuntimeException("graphRules level not supported: " + Sts.opts.graphRules);
				LOG.info("## " + ent._1 + " " + entityScore)
				if( entityScore > maxScore)
				{
					maxScore = entityScore
					maxScoreEntity = ent._1
				}
			})
			LOG.info(">> " +  maxScoreEntity + " - " + maxScoreEntity + " - " + maxScore)
			//val matchingEnt = Sts.qaEntities.filter(_._2 == "h" + maxScoreEntity).toList
			//maxScoreEntity = matchingEnt.head._1 //use the first. UGLY C&C AND BOXER :@
			Sts.qaAnswer = maxScoreEntity
			return Seq(-7.0);
		}
		else 
			return delegate.prove(constants, declarations, evidence, assumptions, goal)
	}
	def initDS (text:BoxerExpression, goal:BoxerExpression) = 
	{
		Baseline.textPreds = text.getPredicates.groupBy(x => x.name + "#" + x.variable + "#" + x.pos).map(_._2.head)
		Baseline.textRels = text.getRelations.groupBy(x => x.name + "#" + x.event + "#" + x.variable).map(_._2.head)
		Baseline.hypPreds = goal.getPredicates.groupBy(x => x.name + "#" + x.variable + "#" + x.pos).map(_._2.head)
		Baseline.hypRels = goal.getRelations.groupBy(x => x.name + "#" + x.event + "#" + x.variable).map(_._2.head)

		Baseline.textEntities = (Baseline.textPreds.map(_.variable.name) ++ Baseline.textRels.flatMap(r => List(r.variable.name, r.event.name))).toSet.toList
		Baseline.textEntitiesMap = Baseline.textEntities.map(e => (e -> Baseline.textPreds.filter(_.variable.name == e).toSet)).toMap

		Baseline.hypEntities = (Baseline.hypPreds.map(_.variable.name) ++ Baseline.hypRels.flatMap(r => List(r.variable.name, r.event.name))).toSet.toList
		Baseline.hypEntitiesMap = Baseline.hypEntities.map(e => (e -> Baseline.hypPreds.filter(_.variable.name == e).toSet)).toMap

		//entities extracted from predicates should contain all entities in relations too
		Baseline.textRels.foreach(r => {
			require(Baseline.textEntities.contains(r.event.name), "Entity 1 %s is not in text %s".format(r.event, Baseline.textEntities));
			require(Baseline.textEntities.contains(r.variable.name), "Entity 2 %s is not in text %s".format(r.variable, Baseline.textEntities));
		})
		Baseline.hypRels.foreach(r => {
			require(Baseline.hypEntities.contains(r.event.name), "Entity %s is not in hypothesis  %s".format(r.event, Baseline.hypEntities));
			require(Baseline.hypEntities.contains(r.variable.name), "Entity %s is not in hypothesis %s".format(r.variable, Baseline.hypEntities));
		})

		//find potential matched entities 
		Baseline.entityPotentialMatchs = Baseline.hypEntities.map(hypE => (hypE -> Baseline.textEntities.filter(textE => {
			//two entities match if they share a predicate with the same lemma
			val textWords = Baseline.textEntitiesMap(textE).map(_.name)
			val hypWords = Baseline.hypEntitiesMap(hypE).map(_.name)
			var x = textWords intersect hypWords
			x = x -- Set("male", "female", "topic") //ignore meta words
			//if (hypWords != tmp)
			//  tmp = hypWords
			var y = Set[String]();

			if (hypWords.contains("@placeholder"))
				y = Sts.qaEntities.keySet intersect textWords
			//println (hypWords.toString + " --- " + textWords.toString + " --- " + x.toString)
			!(x.isEmpty && y.isEmpty)
			//TODO: add potential matches using WordNet, DistSim and LexicalEnt
		}))).toMap

		//println(entityPotentialMatchs)
		//val rules: ListBuffer[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] = ListBuffer();

		Baseline.hypGraph = scalax.collection.mutable.Graph[String, LUnDiEdge]();
		Baseline.hypEntities.foreach(e => Baseline.hypGraph += e)
		Baseline.hypRels.foreach(r => {
			val edge = (r.event.name ~+ r.variable.name)(r);
			//ignore this check, but still need to ckeck that the code below will still work without it
			//require( !hypGraph.contains(edge), "Edge " + edge + " already exists in graph " + hypGraph)
			Baseline.hypGraph += edge
		});

		Baseline.textGraph = scalax.collection.mutable.Graph[String, LUnDiEdge]();
		Baseline.textEntities.foreach(e => Baseline.textGraph += e)
		Baseline.textRels.foreach(r => {
			val edge = (r.event.name ~+ r.variable.name)(r);
			//Ignore this check because it is wrong, and because the code below will still work without it.
			//require( !textGraph.contains(edge), "Edge " + edge + " already exists in graph " + textGraph)
			Baseline.textGraph += edge
		});
	}
	
	def entityScoreLevel1 (entityToEval:String, placeholderNode:Graph[String, LUnDiEdge]#NodeT) : Double =  
	{
		//Why it does not compile without the following two lines ? 
		//why can not it use the this.hypGraph and this.textGraph directly ?
		val hypGraph = Baseline.hypGraph
		val textGraph = Baseline.textGraph

		val textEntityInstances = Baseline.textPreds.filter ( _.name == entityToEval)
		var entityScore = 0.0;
		hypGraph.nodes.foreach(hypNode => 
		{
			val textWords = Baseline.entityPotentialMatchs(hypNode.toString)
			var maxWordScore = 0.0;
			var maxWordPath:String = "";
			val hypSp = (hypNode shortestPathTo placeholderNode.asInstanceOf[hypGraph.NodeT])
			if (hypNode != placeholderNode && !hypSp.isEmpty )
			{
				textEntityInstances.foreach(te => 
				{
					val entityNode = textGraph.get(te.variable.name)
					textWords.foreach(textWord => {
						val textSp = textGraph.get(textWord) shortestPathTo entityNode
						if (!textSp.isEmpty)
						{
							var flag = "neg";
							if (entityToEval == Sts.qaRightAnswer)
								flag = "pos"
									
							val (score, path) = ruleScore (textSp.get, hypSp.get, flag);
							if (score > maxWordScore)
							{
								maxWordScore = score
								maxWordPath = path
							}
						}
					})
				})
				if (maxWordScore > 0)
					println (maxWordPath)
				//LOG.trace(" >> " + Baseline.hypEntitiesMap(hypNode).map(_.name).mkString(", ") + " " + maxWordScore)
				entityScore = entityScore + maxWordScore
			}
		})
		return entityScore;
	}
	def ruleScore(textPath:Graph[String, LUnDiEdge]#Path, hypPath:Graph[String, LUnDiEdge]#Path, flag:String): (Double, String) = 
	{
		val hypGraph = Baseline.hypGraph
		val textGraph = Baseline.textGraph
		val hypSp = hypPath.asInstanceOf[hypGraph.Path]
		val textSp = textPath.asInstanceOf[textGraph.Path]

		var prettyLhs = Baseline.textEntitiesMap( textSp.nodes.head.value).map(_.name).toList.sorted.mkString("_") //+ "(" + textSp.nodes.head.value + ") "
		var lhs = (textSp.edges.toList zip textSp.nodes.tail.toList)
				.map(x => {
					var dir = "";
					if (x._1.edge._1 == x._2)
						dir = "r2l";
					if (x._1.edge._2 == x._2)
						dir = "l2r";
					val s = x._1.label.asInstanceOf[BoxerRel].name	+ "$" + dir
					prettyLhs = prettyLhs + " " + s + " " + Baseline.textEntitiesMap( x._2.value).map(_.name).toList.sorted.mkString("_") //+ "(" + x._2.value + ") "
					s;
				}).toList.sorted.mkString(", ")

		if (textSp.edges.size == 0)
			lhs = "null";
		var prettyRhs = Baseline.hypEntitiesMap( hypSp.nodes.head.value).map(_.name).toList.sorted.mkString("_") // + "(" + hypSp.nodes.head.value + ") "
		var rhs = (hypSp.edges.toList zip hypSp.nodes.tail.toList)
				.map(x => {
					var dir = "";
					if (x._1.edge._1 == x._2)
						dir = "r2l";
					if (x._1.edge._2 == x._2)
						dir = "l2r";
					x._1.label.asInstanceOf[BoxerRel].name	+ "$" + dir
					val s = x._1.label.asInstanceOf[BoxerRel].name	+ "$" + dir
					prettyRhs = prettyRhs + " " + s + " " + Baseline.hypEntitiesMap( x._2.value).map(_.name).toList.sorted.mkString("_") // + "(" + x._2.value + ") "
					s;
				}).toList.sorted.mkString(", ")
 
		if (hypSp.edges.size == 0)
			rhs = "null";
		var path = flag + " - " + prettyLhs + " => " + prettyRhs + " - " + lhs + " - " + rhs 
		var score = 1.0/(1+Math.abs(textSp.weight - hypSp.weight));
		if (Baseline.model != null) //Get score from the trained model, not from distance
		{
			val f = Baseline.featurize(lhs.split(", "), rhs.split(", "));
			val dec_values = new Array[Double](Baseline.model.getNrClass());
			val prediction:Double = Linear.predictProbability(Baseline.model, f, dec_values);
			//println(dec_values)
			score = dec_values(1);
		}
		return (score, path)
	}
	
	def entityScoreLevel2 (entityToEval:String, hypEntitiesSortedInput:List[Graph[String, LUnDiEdge]#NodeT]) : Double =  
	{
		val hypGraph = Baseline.hypGraph
		val textGraph = Baseline.textGraph
		val hypEntitiesSorted = hypEntitiesSortedInput.asInstanceOf[List[hypGraph.NodeT]];
		//println(hypEntitiesSorted)
		val hypFrom = hypEntitiesSorted.head //first node is the placeholder
		val textEntityInstances = Baseline.textPreds.filter ( _.name == entityToEval).map(_.variable.name).toList
		var sumRulesScores = 0.0;

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

				hypSp = path(currentIdx) shortestPathTo hypTo

				var textFroms = Baseline.entityPotentialMatchs(hypSp.get.nodes.head.value);
				if (hypSp.get.nodes.head == hypFrom) //placeholder
					textFroms = textEntityInstances //entity instances (no coref)
				val textTos = Baseline.entityPotentialMatchs(hypSp.get.nodes.last.value);

				var bestRule: String = ""
				var bestRuleScore = 0.0;
				textFroms.foreach(textFrom => 
				{
					textTos.foreach(textTo => 
					{
						val textSp = textGraph.get(textFrom) shortestPathTo textGraph.get(textTo)
						if (textSp.isDefined)
						{
							var flag = "neg";
							if (entityToEval == Sts.qaRightAnswer)
								flag = "pos"
							val (score, path) = ruleScore (textSp.get, hypSp.get, flag);
							
							if (score > bestRuleScore)
							{
								bestRuleScore = score
								bestRule = path
							}
						}
							
					})
				})
				if (bestRuleScore > 0)
				{
					val prettyRule = bestRule.split(" - ")(1);
					if (!rulesSet.contains(prettyRule))
					{
						println("BaselineRule: " + prettyRule)
						rulesSet.add(prettyRule);
					}
					//else println ("BaselineRule: rule skipped")
					anchorNodes.add(hypTo)
					sumRulesScores = sumRulesScores + bestRuleScore
				}
			}
		})
		return sumRulesScores;
	}
	val rulesSet:scala.collection.mutable.Set[String] = scala.collection.mutable.Set();

	//topological sort of entities of hypGraph starting from placeholderNode
	def topologicalSort (placeholderNode:Graph[String, LUnDiEdge]#NodeT) : List[Graph[String, LUnDiEdge]#NodeT] =
	{
		val hypGraph = Baseline.hypGraph
		val nodeQueue = new Queue[hypGraph.NodeT];
		val visitedNodes = new ListBuffer[hypGraph.NodeT];
		nodeQueue.enqueue(placeholderNode.asInstanceOf[hypGraph.NodeT]);
		while (!nodeQueue.isEmpty)
		{
			val currentNode = nodeQueue.dequeue();
			// add currentVar to visited
			visitedNodes += currentNode;
			//find not visited neighbor nodes of currentNode
			val nextNodes = currentNode.neighbors.diff(visitedNodes.toSet);
			//enqueue nextNodes
			nextNodes.foreach(nodeQueue.enqueue(_));
		}
		return visitedNodes.toList.asInstanceOf[List[Graph[String, LUnDiEdge]#NodeT]];
	}


}