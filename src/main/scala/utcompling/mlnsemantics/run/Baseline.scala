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
import util.Word2Vec
import scala.collection.mutable.ListBuffer
import utcompling.mlnsemantics.wordnet.Wordnet
import utcompling.mlnsemantics.wordnet.WordnetImpl

object Baseline
{

	private val LOG = LogFactory.getLog(classOf[Baseline])

	def simpleFeature(fVal:Double): List[(Int, Double)] = 
	{
		val f = (featureIndexShift, fVal)
		featureIndexShift = featureIndexShift + 1
		val zeroVals = List(java.lang.Double.NEGATIVE_INFINITY, java.lang.Double.POSITIVE_INFINITY, 0, -0);
		if (zeroVals.contains(fVal) || java.lang.Double.isNaN(fVal))
			return List()
		return List(f);
	}
	def binnableFeature(fVal:Double, fValMax:Double): List[(Int, Double)] = 
	{
		assert (java.lang.Double.isNaN(fVal) || (fValMax > 0 && fVal <= (fValMax*1.000001) && fValMax >= 0), "%f, %f".format(fVal, fValMax));
		val f = ListBuffer[(Int, Double)]();
		f ++= simpleFeature(fVal);
		val steps = 10;
		for(i <- 0 to steps)
		{
			val from = 1.0*(i-1)*fValMax/steps;
			val to = 1.0*i*fValMax/steps;
			f ++= simpleFeature(if (fVal > from && fVal <= to && fVal != fValMax) 1  else 0);
		}
		f ++= simpleFeature(if (fVal == fValMax) 1 else 0);
		f.toList
	}
	def phraseFeatures (feat: List[List[Double]], pairFeatFun:(String, String) =>List[Double], op:List[Double] => Double, binning:Boolean = false) : List[(Int, Double)] = 
	{
		val doubles = 
		{
			if (feat.length == 0)
				pairFeatFun("", "").map( v => Double.NaN) //dummy input just to know the length of the feature vector 
														//to advance the feature index accordingly
														//the function simpleFeature will discard the NaN anyway 
			else
			{
				val y = feat.transpose
				val z = y.map(op)
				z
			}
		}
		if (binning)
			return doubles.flatMap( d => binnableFeature(d, 1))
		else return doubles.flatMap(simpleFeature) 
	}
	def wordformFeatures(w1: String, w2:String): List[Double] =
	{
		val f = ListBuffer[Double]()
		if (w1 == w2)
			f += 1.0
		else f+= 0.0
		
		//TODO: POS and singular/plural features 
		return f.toList
	}

	def wordnetFeatures(w1: String, w2:String): List[Double] =
	{
		val f = ListBuffer[Double]()
		val w1Synset = if (w1 == "") List() else wordnet.synsets(w1)
		val w2Synset = if (w2 == "") List() else wordnet.synsets(w2)
		if (w1Synset.length == 0) f += 1 else f += 0 //OOV
		if (w2Synset.length == 0) f += 1 else f += 0 //OOV
		if (!(w1Synset.map(wordnet.allHypernyms) intersect w2Synset).isEmpty) f += 1 else f += 0 //hypernym
		if (!(w2Synset.map(wordnet.allHypernyms) intersect w1Synset).isEmpty) f += 1 else f += 0 //hyponym
		if (!(w1Synset intersect w2Synset).isEmpty) f += 1 else f += 0 //same synset
		if (!(w1Synset.map(wordnet.antonyms) intersect w2Synset).isEmpty) f += 1 else f += 0 //antonyms
	
		//TODO: POS and singular/plural features 
		return f.toList
	}
	
	def pathFeatures(featureString: Array[String]): List[(Int, Double)] =
	{
		val f:List[(Int, Double)] = featureString
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

									(id + featureIndexShift + directionShift, 1.0);
								})
								.toList
								.sortBy(_._1)
		featureIndexShift = featureIndexShift + 2*relNames.length
		return f;
	}

	var p = 0; //max 11
	var l = 0; //max 30
	var r = 0; //max 13
	var featureIndexShift = -1;
	//open nsubj$l2r NE:hx1001_first => open dep$r2l pioneer nsubj$l2r w-hotel nmod$l2r NE:@placeholder
	def featurize(rule:String):List[(Int, Double)] =
	{
		val splits = rule.split(" => ")
		assert (splits.size == 2)
		var lhsWords = splits(0).split(" ").zipWithIndex.filter(_._2 % 2 == 0).map(_._1);
		val lhsRels = splits(0).split(" ").zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
		assert (lhsWords.length == (1+lhsRels.length))
		lhsWords = lhsWords.flatMap(_.split("_"))
		
		var rhsWords = splits(1).split(" ").zipWithIndex.filter(_._2 % 2 == 0).map(_._1);
		val rhsRels = splits(1).split(" ").zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
		assert (rhsWords.length == (1+rhsRels.length))
		rhsWords = rhsWords.flatMap(_.split("_"))
		val (pairs, remLhs, remRhs) = align(lhsWords, rhsWords);
		p = Math.max(p, pairs.length)
		l = Math.max(l, remLhs.length)
		r = Math.max(r, remRhs.length)
		//println (pairs.mkString(" *** "))
		//println (remLhs.mkString(" *** "))
		//println (remRhs.mkString(" *** "))
		
		//println (remRhs.mkString("\n"))
		//if (remRhs.contains("") || remRhs.contains(" "))
		//	println("empty str")
		def max(a:List[Double]) : Double = 
			a.max
		def min(a:List[Double]) : Double = 
			a.min
		def avg(a:List[Double]) : Double =
		{
			if (a.length == 0)
				return 0
			else return a.sum*1.0/a.length
		}
		val wordformFeat = pairs.map(p=>wordformFeatures(p._1, p._2));
		val wordnetFeat = pairs.map(p=>wordnetFeatures(p._1, p._2));
		
		featureIndexShift = 1;
		(  simpleFeature(lhsRels.length)
		++ simpleFeature(rhsRels.length)

		++ phraseFeatures (wordformFeat, wordformFeatures, max)
		++ phraseFeatures (wordformFeat, wordformFeatures, min)
		++ phraseFeatures (wordformFeat, wordformFeatures, avg, true)
		
		++ phraseFeatures (wordnetFeat, wordnetFeatures, max)
		++ phraseFeatures (wordnetFeat, wordnetFeatures, min)
		++ phraseFeatures (wordnetFeat, wordnetFeatures, avg, true)

		++ simpleFeature(lhsRels.toSet.intersect(rhsRels.toSet).toList.length)
		++ simpleFeature(lhsRels.toSet.diff(rhsRels.toSet).toList.length)
		++ simpleFeature(rhsRels.toSet.diff(lhsRels.toSet).toList.length)
		
		++ simpleFeature(lhsWords.length)
		++ simpleFeature(rhsWords.length)
		++ simpleFeature(lhsWords.length - rhsWords.length)
		
		++ simpleFeature(remLhs.length)
		++ simpleFeature(remRhs.length)
		
		++ simpleFeature(remLhs.filter(_.startsWith("@entity")).length)
		++ simpleFeature(remRhs.filter(_.startsWith("@entity")).length)
		++ simpleFeature(remLhs.filterNot(_.startsWith("@entity")).length)
		++ simpleFeature(remRhs.filterNot(_.startsWith("@entity")).length)

		++ simpleFeature(pairs.length)

		++ binnableFeature(pairs.length*2*1.0/(lhsWords.length+rhsWords.length), 1)
		
		++ binnableFeature(pairs.length*1.0/lhsWords.length, 1)
		++ binnableFeature(pairs.length*1.0/rhsWords.length, 1)
		
		++ binnableFeature((pairs.map(_._3) :+ 0.0).max, 1)
		++ binnableFeature((pairs.map(_._3) :+ 1.0).min, 1)
		++ simpleFeature(pairs.map(_._3).sum)
		++ binnableFeature(pairs.map(_._3).sum *1.0 / pairs.length, 1)

		++ pathFeatures(lhsRels) 
		++ pathFeatures(rhsRels) 
		++ pathFeatures(lhsRels.toSet.intersect(rhsRels.toSet).toArray)
		++ pathFeatures(lhsRels.toSet.diff(rhsRels.toSet).toArray)
		++ pathFeatures(rhsRels.toSet.diff(lhsRels.toSet).toArray)
		
		)
	}
	def align(lhs:Array[String], rhs:Array[String]):(List[(String, String, Double)], List[String], List[String]) = 
	{
		var lhsWords = ListBuffer() ++ lhs
		var rhsWords = ListBuffer() ++ rhs
		var continue = true;
		var pairs:ListBuffer[(String, String, Double)] = ListBuffer();
		while (continue)
		{
			continue = false ;
			var maxPairIdx = (-1, -1);
			var maxSim = -1.0;
			lhsWords.indices.foreach(lhsIdx => {
				rhsWords.indices.foreach(rhsIdx => {
					var cosine = vectorSpace.getOrZero(lhsWords(lhsIdx)) cosine vectorSpace.getOrZero(rhsWords(rhsIdx))
					if (lhsWords(lhsIdx) == rhsWords(rhsIdx) && rhsWords(rhsIdx).startsWith("@entity"))
						cosine = 1.0;
					if (cosine > 0.01 && cosine > maxSim)
					{
						maxSim = cosine
						maxPairIdx = (lhsIdx, rhsIdx)
					}
				})
			})
			if (maxSim > 0)
			{
				continue = true; // a match found
				pairs += ((lhsWords(maxPairIdx._1), rhsWords(maxPairIdx._2), maxSim))
				lhsWords.remove(maxPairIdx._1)
				rhsWords.remove(maxPairIdx._2)
			}
		}
		return (pairs.toList, lhsWords.toList, rhsWords.toList);
	}

	def train (features:ListBuffer[List[(Int, Double)]], labels:ListBuffer[Double], featuresCount:Int, modelPath:String) : Any = 
	{
		assert (features.length == labels.length)
		//parameter.setWeights(Array(1.0/positiveCounter, 1.0/(features.length - positiveCounter)), Array(1, -1))
		val positiveCounter = labels.filter(_ == 1.0).length;
		val negativeCounter = features.length - positiveCounter
		val posW = 1.0*features.length/positiveCounter
		val negW = 1.0*features.length/negativeCounter;
		println(posW  + " -- " + negW)

		if (Baseline.isSVM)
		{
			val problem = new svm_problem();
			problem.l = features.length; //number of data points
			problem.x = features.map(e => e.map(f => {
				assert (f._1 <= featuresCount);
				val n = new svm_node();
				n.index = f._1;
				n.value = f._2;
				n;
			}).toArray ).toArray //features
			problem.y = labels.toArray // target values
			
			
			val parameter:svm_parameter = new svm_parameter();
			
			parameter.svm_type = svm_parameter.C_SVC;
			parameter.kernel_type = svm_parameter.SIGMOID;
			parameter.degree = 3;
			parameter.gamma = 0;	// 1/num_features
			parameter.coef0 = 0;
			parameter.nu = 0.5;
			parameter.cache_size = 500;
			parameter.C = 1;
			parameter.eps = 1e-3;
			parameter.p = 0.1;
			parameter.shrinking = 1;
			parameter.probability = 0;
			parameter.nr_weight = 2;
			
			parameter.weight = Array(posW, negW);
			parameter.weight_label = Array(1, -1);
			var model:svm_model = svm.svm_train(problem, parameter);
			svm.svm_save_model(modelPath, model)
			// load model or use it directly
			model = svm.svm_load_model(modelPath)
			//println ("number of SV: " + model.nSV.toList.mkString(", "))
			return model;
		}
		else
		{
			val problem = new de.bwaldvogel.liblinear.Problem();
			problem.l = features.length; //number of data points
			problem.n = featuresCount
			problem.x = features.map(e => e.map(f => {
				assert (f._1 <= featuresCount);
				new FeatureNode(f._1, f._2).asInstanceOf[Feature]
			}).toArray ).toArray //features 
			problem.y = labels.toArray // target values
			
			val solver:SolverType = SolverType.L2R_LR
			val C:Double = 1.0;    // cost of constraints violation
			val eps:Double = 0.0001; // stopping criteria
			
			val parameter:Parameter = new Parameter(solver, C, eps);
			parameter.setWeights(Array(posW, negW), Array(1, -1))
			var model:Model = Linear.train(problem, parameter);
			val modelFile:File = new File(modelPath);
			model.save(modelFile);
			// load model or use it directly
			model = Model.load(modelFile);
			println(model.getFeatureWeights().toList.zipWithIndex.flatMap ( w => if (w._1 == 0.0 )None else Some("%d, %2.4f".format(w._2, w._1))).mkString("\t"))
			return model;
		}
	}
	
	def predictProbability (model:Any, f:List[(Int, Double)]) : (Int, Array[Double]) =  // (label, Array[probabilities])
	{
		if (Baseline.isSVM)
		{
			val m = model.asInstanceOf[svm_model];
			val dec_values = new Array[Double](m.nr_class);
			val prediction = svm.svm_predict_probability(m, f.map(p => {
				val n = new svm_node();
				n.index = p._1;
				n.value = p._2;
				n;
			}).toArray, dec_values).toInt
			return (prediction, dec_values);
		}
		else
		{
			val m = model.asInstanceOf[Model];
			val dec_values = new Array[Double](m.getNrClass());
			val prediction = Linear.predictProbability(m, f.map(p => new FeatureNode(p._1, p._2).asInstanceOf[Feature]).toArray, dec_values).toInt;
			return (prediction, dec_values);
		}
	}
	
	def predict(model:Any, f:List[(Int, Double)]) : Int = 
	{
		val (prediction, probabilities) = predictProbability(model, f);
		return prediction;
	}

	def main(args: Array[String]) 
	{
		//println(wordnetFeatures("", ""))
		featureIndexShift = 1;
		//println(simpleFeature(Double.NaN))
		//println(simpleFeature(0*1.0/0))
		//return
		/*
		val v = 0.999999999999999999999999999999999999999999999999999999999
		val max = 1
		val steps = 10
		for (i <- 0 to steps )
		{
			val from =  1.0*(i-1)*max/steps
			val to = 1.0*i*max/steps
			println(from + " -- " + to  + " -- " + (if (v > from && v <= to && v != max) 1  else 0) )
		}
		return;
		*/
		//println(args.mkString(", "))
		Logger.getRootLogger.setLevel(Level.OFF)
		assert(args.length == 4)
		val trainOrTest = args(0);
		assert (List("train", "test", "generate").contains(trainOrTest));
		val modelPath = args(1); //save a model or read a saved model
		val inputFile = args(2); //could be training or testing file
		val word2vecModelPath = args(3); //save a model or read a saved model
		if (trainOrTest == "generate")
			vectorSpace = BowVectorSpace(word2vecModelPath)
		var positiveCounter = 0;
		val positiveEntries:ListBuffer[String] = ListBuffer[String]();
		if (args(0) == "train")
		{
			for (line <- Source.fromFile(inputFile).getLines) 
			{
				val splits = line.split (" ");
				val label = splits(0).toDouble.toInt
				val featString = splits.tail.mkString(" ")
				if (label == 1.0)
					positiveEntries.+=(featString);
			}
		}
		var features:ListBuffer[List[(Int, Double)]] = ListBuffer[List[(Int, Double)]]()
		var maxF:Array[Double] = null;
		var minF:Array[Double] = null;
		val labels:ListBuffer[Double] = ListBuffer()
		val featureLabelMap:scala.collection.mutable.Map[String, (Int, Int)] = scala.collection.mutable.Map[String, (Int, Int)]();
		var deletedNegativeExamples = 0;
		for (line <- Source.fromFile(inputFile).getLines) 
		{
			if (args(0) == "generate")
			{
				val splits = line.split ("\t");
				assert (splits.length == 3);
				assert (List("neg", "pos").contains(splits(0)));
				var label = -1.0;
				if (splits(0) == "pos")
					label = 1.0;
	
				if (true) //label == 1.0 || rand.nextInt(5) == 0
				{
					//println(line)
					val f:List[(Int, Double)] = featurize(splits(2));
					val featString = f.map(x => x._1 + ":" + "%1.4f".format(x._2)).mkString(" ")
					var stat = featureLabelMap.getOrElse(featString, (0, 0));
					if (label == 1.0)
						stat = (stat._1+1, stat._2)
					else if (label == -1.0)
						stat = (stat._1, stat._2+1)
					else throw new RuntimeException("Unexpected label: " + label)
					featureLabelMap.put(featString, (stat._1, stat._2))
					println (label+" " + featString)
					features.append(f)
					labels.+=(label);
					if (label == 1.0)
						positiveCounter = positiveCounter + 1;
				}
			}
			else if (args(0) == "train")
			{
				val splits = line.split (" ");
				val label = splits(0).toDouble.toInt
				val featString = splits.tail.mkString(" ")
				if (positiveEntries.contains(featString) && label == -1.0 )
					deletedNegativeExamples = deletedNegativeExamples + 1;
				else
				{
					val f:List[(Int, Double)] = splits.tail.map( ft => {
						val Array(idx, fVal) = ft.split(":")
						featureIndexShift = Math.max(featureIndexShift, idx.toInt);
						(idx.toInt, fVal.toDouble);
					}).toList
	
					var stat = featureLabelMap.getOrElse(featString, (0, 0));
					if (label == 1.0)
						stat = (stat._1+1, stat._2)
					else if (label == -1.0)
						stat = (stat._1, stat._2+1)
					else throw new RuntimeException("Unexpected label: " + label)
					featureLabelMap.put(featString, (stat._1, stat._2))
					//println (label+" " + featString)
					features.append(f)
					labels.+=(label);
					if (label == 1.0)
						positiveCounter = positiveCounter + 1;
				}
			}
		}
		assert(features.length == labels.length)
		if (args(0) == "train")
		{
			println (features.length + " -- " + labels.length)
			println (" >>>> " + Baseline.p + ", " + Baseline.l + ", " + Baseline.r  )
			println ("deletedNegativeExamples: " + deletedNegativeExamples );
			var bestCorrect = 0;
			var bestTotal = 0;
			var bestTruePositive = 0
			var bestPredictedPositive = 0;
			var bestActualPositive = 0;
			//Map(rule -> (count of positive, count of negative))
			featureLabelMap.foreach( f => {
				val posCnt = f._2._1 
				val negCnt = f._2._2
				if (posCnt > negCnt) //positive more than negative 
				{
					//==> predict positive
					bestCorrect = bestCorrect + posCnt;
					bestTruePositive = bestTruePositive + posCnt
					bestPredictedPositive = bestPredictedPositive + posCnt
					bestActualPositive = bestActualPositive + posCnt;
				}
				else //negative more than or equal positive 
				{
					//==> predict negative
					bestCorrect = bestCorrect + negCnt;
					bestTruePositive = bestTruePositive + 0
					bestPredictedPositive = bestPredictedPositive + 0
					bestActualPositive = bestActualPositive + posCnt;
				}
				bestTotal = bestTotal + posCnt + negCnt;
			})
			println ("Best Accuracy: " + bestCorrect + " / " + bestTotal + " = " + bestCorrect*1.0/bestTotal)
			var bestP = bestTruePositive*1.0/bestPredictedPositive
			var bestR = bestTruePositive*1.0/bestActualPositive
			println ("Best Precision: " + bestTruePositive + " / " + bestPredictedPositive + " = " + bestP)
			println ("Best Recall: " + bestTruePositive + " / " + bestActualPositive + " = " + bestR)
			println ("Best F1: " + 2.0 * bestP * bestR / (bestP + bestR))
	
			//Begin Scale
			maxF = Array.fill[scala.Double](featureIndexShift+1)(1.0*scala.Int.MinValue);
			minF = Array.fill[scala.Double](featureIndexShift+1)(1.0*scala.Int.MaxValue);
			features.foreach( f => f.foreach( p  =>{
				maxF(p._1) = Math.max(maxF(p._1), p._2)
				minF(p._1) = Math.min(minF(p._1), p._2)
			}))
			//End Scale
			
			val model:Any = train (features, labels, featureIndexShift, modelPath);
	
			/////
			var correct = 0;
			var total = 0;
			var truePositive = 0
			var predictedPositive = 0;
			var actualPositive = 0;
	
			(features zip labels).foreach( t => { 
				val prediction:Int = Baseline.predict(model, t._1);
				if (prediction == t._2)
					correct = correct + 1;
				if (prediction == t._2 && prediction == 1)
					truePositive = truePositive + 1
				if (prediction == 1)
					predictedPositive = predictedPositive + 1
				if (t._2 == 1)
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
	val isSVM:Boolean = true
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
	var model:Object = null;
	var preEvalGraphRules:collection.mutable.Map[String, (Int, Int)] = null;
	var preEvalScoredGraphRules:collection.mutable.Map[String, Double] = null;
	var vectorSpace:BowVectorSpace = null;

	val rand = scala.util.Random
	rand.setSeed(42)
	val wordnet:WordnetImpl = new WordnetImpl();
	/////
	def ruleScore(textPath:Graph[String, LUnDiEdge]#Path, hypPath:Graph[String, LUnDiEdge]#Path): (Double, String) = 
	{
		val hypGraph = Baseline.hypGraph
		val textGraph = Baseline.textGraph
		val hypSp = hypPath.asInstanceOf[hypGraph.Path]
		val textSp = textPath.asInstanceOf[textGraph.Path]
		def reanonymize (w:String):String = 
		{
			/*if (Sts.qaEntities.contains(w))
				"NE:" + Sts.qaEntities(w).toList.sorted.head
			else w
			* 
			*/
			w
		}
		var prettyLhs = Baseline.textEntitiesMap( textSp.nodes.head.value).map(w=>reanonymize(w.name)).toList.sorted.mkString("_") //+ "(" + textSp.nodes.head.value + ") "
		var lhs = (textSp.edges.toList zip textSp.nodes.tail.toList)
				.map(x => {
					var dir = "";
					if (x._1.edge._1 == x._2)
						dir = "r2l";
					if (x._1.edge._2 == x._2)
						dir = "l2r";
					val s = x._1.label.asInstanceOf[BoxerRel].name	+ "$" + dir
					prettyLhs = prettyLhs + " " + s + " " + Baseline.textEntitiesMap(x._2.value).map(w=>reanonymize(w.name)).toList.sorted.mkString("_") //+ "(" + x._2.value + ") "
					s;
				}).toList.sorted.mkString(", ")

		if (textSp.edges.size == 0)
			lhs = "null";
		var prettyRhs = Baseline.hypEntitiesMap( hypSp.nodes.head.value).map(w=>reanonymize(w.name)).toList.sorted.mkString("_") // + "(" + hypSp.nodes.head.value + ") "
		var rhs = (hypSp.edges.toList zip hypSp.nodes.tail.toList)
				.map(x => {
					var dir = "";
					if (x._1.edge._1 == x._2)
						dir = "r2l";
					if (x._1.edge._2 == x._2)
						dir = "l2r";
					x._1.label.asInstanceOf[BoxerRel].name	+ "$" + dir
					val s = x._1.label.asInstanceOf[BoxerRel].name	+ "$" + dir
					prettyRhs = prettyRhs + " " + s + " " + Baseline.hypEntitiesMap(x._2.value).map(w=>reanonymize(w.name)).toList.sorted.mkString("_") // + "(" + x._2.value + ") "
					s;
				}).toList.sorted.mkString(", ")
 
		if (hypSp.edges.size == 0)
			rhs = "null";
		var path = prettyLhs + " => " + prettyRhs //+ " - " + lhs + " - " + rhs

		//if (vectorspace.numDims > 0) // a vectorspace is provided
		//	rw = ruleWeighter.weightForRules(lhsText, List(), Seq((rhsText, List())).toMap, vectorspace).head._2.get; //rule weight based on dist sim

		var score:Double =
		if (Sts.opts.emRandInit)
			Baseline.rand.nextDouble
		else 
			1.0/(1+Math.abs(textSp.weight - hypSp.weight));
		
		if (Baseline.preEvalGraphRules != null)
		{
			val (posCnt, negCnt) = Baseline.preEvalGraphRules.getOrElse(path /*+ " - " + Sts.pairIndex*/, (0, 0));
			if ((negCnt + posCnt) > 0) //rule exist in the paraphrases file
			{
				if (posCnt > 0 && negCnt > 0)
					println( "rule with mixed labels: %d pos, %d neg".format(posCnt, negCnt) )
				//if (posCnt > negCnt) score = 1;
				if (posCnt > 0) score = 1;
				else score = 0;
			}
			else println ("Expected rule not found: " + path);
		}
		if (Baseline.preEvalScoredGraphRules != null)
		{
			val scoreOption = Baseline.preEvalScoredGraphRules.get(path)
			if (scoreOption.isDefined)
				score = scoreOption.get
			else println ("Expected rule not found: " + path);
		}
		
		if (Baseline.model != null) //Get score from the trained model, not from distance
		{
			//val f = Baseline.featurize(lhs.split(", "), rhs.split(", "));
			val f = Baseline.featurize(prettyLhs + " => " + prettyRhs);
			//val dec_values = new Array[Double](Baseline.model.getNrClass());
			val dec_values = Baseline.predictProbability(Baseline.model, f)._2;
			//println ("###" + prediction + " -- " + dec_values.toList.mkString(", "))
			score = dec_values(0);
		}
		return (score, path)
	}
	//
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
			val nextNodes = currentNode.neighbors.diff(visitedNodes.toSet).diff(nodeQueue.toSet)
			//enqueue nextNodes
			nextNodes.toList.sortBy(_.value).foreach(nodeQueue.enqueue(_));
		}
		return visitedNodes.toList.asInstanceOf[List[Graph[String, LUnDiEdge]#NodeT]];
	}

}
class Baseline (delegate: ProbabilisticTheoremProver[BoxerExpression])
	extends ProbabilisticTheoremProver[BoxerExpression] 
{

	
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
		Baseline.vectorSpace = Sts.vectorSpace
		
		//same data structures will be used in GraphRules. 
		initDS (assumptions.head.expression, goal);
		if (Sts.opts.ruleClsModel.isDefined && Baseline.model == null)
			Baseline.model = Model.load(new File(Sts.opts.ruleClsModel.get));
		////

		if (Sts.opts.baseline == "dep")
		{
			var maxScore = 0.0
			var maxScoreEntity = "";
			val placeholderNode = Baseline.hypGraph.get( Baseline.hypPreds.filter ( _.name == "@placeholder" ).head.variable.name )
			//var hypEntitiesSorted:List[Graph[String, LUnDiEdge]#NodeT] = null;
			//if (Sts.opts.graphRules == 2)
			//	hypEntitiesSorted = Baseline.topologicalSort(placeholderNode)
			
			Sts.qaEntities/*.filter ( e => e._2 != "" && e._1 != "@placeholder" )*/.foreach(ent=>
			{
				var entityScore = 0.0;
				if (Sts.opts.graphRules == 1)
				{
					entityScore = entityScoreLevel1(ent._1, placeholderNode);
				}
				/*else if (Sts.opts.graphRules == 2)
				{
					entityScore = entityScoreLevel2(Some(ent._1), hypEntitiesSorted);
				}*/
				else throw new RuntimeException("graphRules level not supported: " + Sts.opts.graphRules);
				Baseline.LOG.info("## " + ent._1 + " " + entityScore)
				if( entityScore > maxScore)
				{
					maxScore = entityScore
					maxScoreEntity = ent._1
				}
			})
			Baseline.LOG.info(">> " +  maxScoreEntity + " - " + maxScoreEntity + " - " + maxScore)
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
		//if given graphRulesFile and preEvalGraphRules and preEvalScoredGraphRules are still null, 
		//then read the file, find which format it is, and store it in preEvalGraphRules or preEvalScoredGraphRules
		if (Sts.opts.graphRulesFile.isDefined && Baseline.preEvalGraphRules == null && Baseline.preEvalScoredGraphRules == null)
		{
			Baseline.preEvalGraphRules = collection.mutable.Map(); 
			Baseline.preEvalScoredGraphRules = collection.mutable.Map();
			val fr  = FileUtils.readLines(Sts.opts.graphRulesFile.get)
			val pos_neg = List("pos", "neg");
			
			fr.foreach(line => {
				val Array(tok0, tok1, r) = line.split("\t");
				println (tok0 + " ## " + tok1 + " ## " + r)
				if (pos_neg.contains(tok0))
				{
					val label = tok0;
					val qIdx = tok1;
					assert (label == "pos" || label == "neg");
					assert (r.contains(" => "));
					var cnts = Baseline.preEvalGraphRules.getOrElse(r  /*+ " - " + qIdx*/, (0, 0));
					if (label == "pos")
						cnts = (cnts._1 + 1, cnts._2)
					else cnts = (cnts._1, cnts._2 + 1)
					Baseline.preEvalGraphRules.put(r /*+ " - " + qIdx*/, cnts);
				}
				else if (pos_neg.contains(tok1))
				{
					val score = tok0.toDouble;
					val label = tok1;
					assert (label == "pos" || label == "neg");
					assert (r.contains(" => "));
					Baseline.preEvalScoredGraphRules.put(r /*+ " - " + qIdx*/, score);
				}
				else throw new RuntimeException("Unsupported format: " + line)
			})

			if (Baseline.preEvalGraphRules.size == 0)
				Baseline.preEvalGraphRules = null
			if (Baseline.preEvalScoredGraphRules.size == 0)
				Baseline.preEvalScoredGraphRules = null
			assert (Baseline.preEvalScoredGraphRules == null || Baseline.preEvalGraphRules == null)
		}
		//
		Baseline.textPreds = text.getPredicates.groupBy(x => x.name + "#" + x.variable /*+ "#" + x.pos*/).map(_._2.head) 
		Baseline.textRels = text.getRelations.groupBy(x => x.name + "#" + x.event + "#" + x.variable).map(_._2.head)
		Baseline.hypPreds = goal.getPredicates.groupBy(x => x.name + "#" + x.variable /*+ "#" + x.pos*/).map(_._2.head)
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
			
			val cosineSimilarities = textWords.flatMap(t => {
				val vt =  Baseline.vectorSpace.getOrZero(t);
				hypWords.map(h => Baseline.vectorSpace.getOrZero(h).cosine(vt))
			}).toList.filter(_ > 1)
			
			var x = textWords intersect hypWords
			x = x -- Set("male", "female", "topic") //ignore meta words
			//if (hypWords != tmp)
			//  tmp = hypWords
			var y = Set[String]();

			if (hypWords.contains("@placeholder"))
				y = Sts.qaEntities.keySet intersect textWords
				
			if (!cosineSimilarities.isEmpty && x.isEmpty && y.isEmpty)
				println ("Cosine similarity rule added: " + textWords.mkString(", ") + " -- " +  hypWords.mkString(", "))
			//println (hypWords.toString + " --- " + textWords.toString + " --- " + x.toString)
			!(x.isEmpty && y.isEmpty && cosineSimilarities.isEmpty) 
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
							val (score, path) = Baseline.ruleScore (textSp.get, hypSp.get);
							if (score > maxWordScore)
							{
								if (maxWordPath != "" && !Sts.opts.emTrainOnBest)
									println ("neg\t" + maxWordPath)

								maxWordScore = score
								maxWordPath = path
							}
							else if (!Sts.opts.emTrainOnBest)
								println ("neg\t" + path)
						}
					})
				})
				if (maxWordScore > 0)
				{
					var flag = "neg";
					if (entityToEval == Sts.qaRightAnswer)
						flag = "pos"
					println (flag + "\t"+ maxWordPath)
				}
				//LOG.trace(" >> " + Baseline.hypEntitiesMap(hypNode).map(_.name).mkString(", ") + " " + maxWordScore)
				entityScore = entityScore + maxWordScore
			}
		})
		return entityScore;
	}
	val rulesSet:scala.collection.mutable.Set[String] = scala.collection.mutable.Set();

}