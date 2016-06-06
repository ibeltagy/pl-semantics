package utcompling.mlnsemantics.run

import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.Pattern.Range
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.mlnsemantics.vecspace._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import org.apache.log4j.Logger
import org.apache.log4j.Level
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.mlnsemantics.inference._
import utcompling.scalalogic.discourse.impl.PreparsedBoxerDiscourseInterpreter
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.mlnsemantics.inference.DependencyParsedBoxerDiscourseInterpreter
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.util.Config
import utcompling.mlnsemantics.util.Lucene
import utcompling.mlnsemantics.datagen.CncLemmatizeCorpusMapper
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.drt.expression.DrtApplicationExpression
import scala.math.{sqrt, pow, min, ceil}
import utcompling.mlnsemantics.inference.NoExistProbabilisticTheoremProver
import utcompling.Resources
import utcompling.mlnsemantics.datagen.Lemmatize
import dhg.depparse.DepParser
import utcompling.mlnsemantics.rules.DiffRules
import java.net.InetAddress

/**
 *
 */

class Sts{
  
}
object Sts {

  private val LOG = LogFactory.getLog(classOf[Sts])
  val Range(defaultRange) = "1-5000"
  val SomeRe = """Some\((.*)\)""".r

  //Global variables used by other classes
  var opts: Config = null;	//command line arugments
  var text = "";			//Text
  var textLemma = "";		//Text lemmatized 
  var hypothesis = "";		//Hypothesis
  var hypothesisLemma = "";	//Hypothesis lemmatized
  var goldStandard:Double = 0;	// Gold standard annotation
  var pairIndex = 0;		//Pair index
  var qaRightAnswer = "";		//Correct answer for QA
  var qaAnswer = "";		//Answer for QA
  var qaEntities = Map[String, String]();		//all entities in the QA pair
  var luceneDistPhrases:Lucene = null;	// Lucene repository of precompiled distributional phrases 
  var luceneParaphrases:List[Lucene] = null;	// Lucene repository of precompiled paraphrases
  var vectorSpace:BowVectorSpace = null; //vectorspace

  var resultOnePair: Seq[Double] = Seq(); //passing result to the adept code
  var depParser:DepParser = null;
  
  def callFromAdept(args: Array[String]): Array[Double] = {
	  main(args);
	  return resultOnePair.toArray;
  }

  def main(args: Array[String]) {
  	val computername = InetAddress.getLocalHost().getHostName();
  	System.out.println("host: " + computername);
  	System.err.println("host: " + computername);
    val (newArgs, optPairs) =
      ("" +: args.toSeq).sliding2.foldLeft((Vector[String](), Vector[(String, String)]())) {
        case ((newArgs, opts), (a, b)) => a match {
          case _ if a.startsWith("-") => (newArgs, opts :+ (a -> b))
          case _ if b.startsWith("-") => (newArgs, opts)
          case _ => (newArgs :+ b, opts)
        }
      }

    println("args: " + optPairs.toMap.mkString(", "))
    opts = new Config(optPairs.toMap);

    Logger.getRootLogger.setLevel(opts.loglevel)

    newArgs.toSeq match {
     
		case Seq("qa", qDir, Range(range)) => runQA (qDir, range)

      case Seq("qa", qDir) => runQA (qDir, defaultRange)

      case Seq("sen", sen1, sen2) => parseRunOnePair(sen1, sen2)
      
      case Seq("lem", stsFile, lemFile) =>
      	println ("No need to generate LEM file anymore");
        /*  
        val sentences = readLines(stsFile).flatMap(_.split("\t")).toVector
        val step = 500;  //file is large. It should be partitioned before passed to the parser
        val totalSen = sentences.length;
        val itrCount = (ceil (totalSen*1.0 / step)).intValue();
        
	        FileUtils.writeUsing(lemFile) { f =>
	          for (i <- 0 to itrCount-1 )
	          {
		          val from  = i * step;
		          val to = min((i+1)*step, totalSen);
					//println (from + ", " +  to);
					 //println (sentences.slice(from, to));
		          val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences.slice(from, to))
		          //println(lemmatized);
		          lemmatized
		            .map(_.map(_.map(_._2).mkString(" ")).getOrElse("______parse_failed______"))
		            .grouped(2).foreach { case Seq(a, b) => f.write("%s\t%s\n".format(a, b)) }
	          }
	        }
	    * 
	    */
        

      case Seq("vs", stsFile, stsVsFile) =>
        val allLemmas = readLines(stsFile).flatMap(_.split("\\s+").map(Lemmatize.lemmatizeWord)).toSet
        val fullVsFile = opts.vectorSpace
        FileUtils.writeUsing(stsVsFile) { f =>
          for (line <- readLines(fullVsFile)) {
            val word = line.split("-.\t|\t")(0)
            if (allLemmas(word) || allLemmas(word.toLowerCase))
              f.write(line + "\n")
          }
        }

      case Seq("box", stsFile, boxFile) =>
        val di = new ModalDiscourseInterpreter()
        val sentences = readLines(stsFile).flatMap(_.split("\t")).map(Tokenize.separateTokens).toList
        val step = 120; //file is large. It should be partitioned before passed to the parser
        val totalSen = sentences.length;
        val itrCount = (ceil (totalSen*1.0 / step)).intValue();   
        writeUsing(boxFile) { f =>
          for (i <- 0 to itrCount-1 )
          {
        	  val from  = i * step;
        	  val to = min((i+1)*step, totalSen);
	          for (x <- di.batchInterpret(sentences.slice(from, to)))
	          {
	            f.write(x + "\n")
	          }
          }
        } 

      case Seq("run", stsFile, boxFile, lemFile, stsVsFile, goldSimFile, outputSimFile) =>
        run(stsFile, boxFile, lemFile, stsVsFile, goldSimFile, outputSimFile, _ => true, defaultRange.toSet)

      case Seq("run", stsFile, boxFile, lemFile, stsVsFile, goldSimFile, outputSimFile, Range(range)) =>
        run(stsFile, boxFile, lemFile, stsVsFile, goldSimFile, outputSimFile, UniversalSet(), range.toSet)

    }
  
	 def runQA (qaDir: String, range: Seq[Int])
	 {
		var totalCount = 0;
		var correctCount = 0;
		val qFiles = new java.io.File(qaDir).listFiles.filter(_.getName.endsWith(".question")).sorted
		for (qFileIndex <- range)
		{
			val qFile = qFiles(qFileIndex - 1)
         println("=============\nQuestion %s\n%s\n=============".format(qFileIndex, qFile))
			val linesReader = readLines(qFile)
         val title = linesReader.next()
         linesReader.next()
         var context = linesReader.next() + " "; //add a space to the end of text to make sure 
									//that each token ends with a space. This makes replacing
									//@entityXX with the entity name easier
         linesReader.next()
         var question = linesReader.next()  + " "  //same, add a space to the end of text. 
         linesReader.next()
         val rightAnswer = linesReader.next()
         assert (linesReader.next() == "");
         if (opts.ner == "gs")
         {
        	context = context.replace(" ", "|O ");
        	question = question.replace(" ", "|O ");
        	question = question.replace ("@placeholder|O", "@placeholder|I-PER")
         }
         Sts.qaEntities = Map ( "@placeholder" -> "");
         while(linesReader.hasNext)
         {
            val splits = linesReader.next().split(":");
            var entityId = splits(0)
            var entityName = splits(1)
            entityName = entityName.replace(" ", "-").toLowerCase();
            if (entityId == rightAnswer)
            	Sts.qaRightAnswer = entityName
            Sts.qaEntities = Sts.qaEntities + (entityName -> "" )
            if (opts.ner == "gs")
            {
            	entityId = entityId + "|O";
            	//entityName = (entityName+" ").replace(" ", "|I-PER ").trim();
            	entityName = entityName + "|I-PER"
            }
            //include a space after entity index to make sure I am replacing a whole token
            context = context.replace (entityId + " ", entityName + " ")
            question = question.replace (entityId + " ", entityName + " ")
         }
         context = context.trim() //a space at the end of the line upsets C&C
         question = question.trim() //a space at the end of the line upsets C&C
         
         assert(Sts.qaRightAnswer != "", "name of the answer entity is not found")
         assert(!context.contains("@entity") && !question.contains("@entity"), "Context or Question have anonymous entities")

		 try
         {
			totalCount = totalCount + 1;
			if (Sts.opts.baseline == "word")
			{
				val words = context.split(" ");
				val qWords = question.split(" ");
				var minCost = 0.0
				var minCostEntity = "";
				Sts.qaEntities.foreach(ent=>
				{
					val e = ent._1; //entity name
					val entityLoc = words.zipWithIndex.filter(_._1 == e).map(_._2) 
					var entityCost = 0.0;
					var placeholderLoc = qWords.indexOf("@placeholder")

					for ( i <- 0 until qWords.length)
					{
						val qWord = qWords(i);
						val wordLoc = words.zipWithIndex.filter(_._1 == qWord).map(_._2)
						var minDist = 0.0;
						
						entityLoc.foreach (idx1 => {
							wordLoc.foreach (idx2 => {
								minDist = Math.max(minDist, 1.0/(1+Math.abs(idx1 - idx2 - placeholderLoc + i)))
								//if ( i == 9):
								//	log(" >> " + str(minDist) + " " + str(idx1) + " " + str(idx2) + " " + str(placeholderLoc) + " " + str(i))
							})
						})
						LOG.trace(" >> " + qWord + " " + minDist)
						entityCost = entityCost + minDist
					}
					LOG.trace("## " + e + " " + entityCost)
					if( entityCost > minCost)
					{
						minCost = entityCost
						minCostEntity = e
					}
				})
				LOG.trace(">> " +  minCostEntity + " - " + minCost)
				Sts.qaAnswer = minCostEntity;
			}
			else
				parseRunOnePair(context, question)
			
			println("GS: " + Sts.qaRightAnswer + " -- " + "Actual: " + Sts.qaAnswer)
			if (Sts.qaAnswer == Sts.qaRightAnswer)
			{
				println ("QA-result: right " + Sts.resultOnePair)
				correctCount = correctCount + 1;
			}
			else 
				println ("QA-result: wrong " + Sts.resultOnePair)
         }
		 catch 
		 {
         	case e: Throwable =>
					LOG.error("Unknowen error" + e.printStackTrace());
					println ("List()")
					if (Sts.qaRightAnswer != "")
						println ("QA-result: error")
		 }
		}
		println ("Accuracy: " + correctCount*1.0/totalCount  + ", correct: " + correctCount + ", total: " + totalCount)
	 } 
    def parseRunOnePair(sen1: String, sen2: String )  = {
		 //val sentences = Array(sen1.toLowerCase(), sen2.toLowerCase()).map(Tokenize.separateTokens).toList
		 val sentences = if (opts.ner != "candc") 
			 				Array(sen1, sen2).toList  //it is already tokenized
			 			else
			 			  	Array(sen1, sen2).map(Tokenize.separateTokens).toList
		 LOG.trace(sentences);
    	 //val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(Array(sen1, sen2))
    	 //val lemmas = lemmatized.map(_.map(_.map(_._2).mkString(" ")).getOrElse("______parse_failed______"))
	     val lemmas:List[String] = sentences.map(Lemmatize.lemmatizeWords)
    	 //println(lemmas);
	     var depParser: dhg.depparse.DepParser = null;
	     val boxes: List[Option[utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression]] = Sts.opts.logicFormSource match 
	     {
				case "dep" => {
				  //depParser = DepParser.load();
				  List();
				}
				case "box" => {
				  val di = new ModalDiscourseInterpreter
				  val sen1Box = di.batchInterpret(List(sentences(0)), verbose = LOG.isTraceEnabled());
				  val sen2Box = di.batchInterpret(List(sentences(1)), verbose = LOG.isTraceEnabled());
				  sen1Box ++ sen2Box
				}
		 }
		 //val boxes = di.batchInterpret(sentences);
	     LOG.trace("boxes: >>>> " + boxes);
	     val allLemmas = lemmas.flatMap(_.split("\\s+")).toSet
	     /*val fullVsFile = opts.vectorSpace
         val tempVSFile = FileUtils.mktemp();
	      FileUtils.writeUsing(tempVSFile) { f =>
          	for (line <- readLines(fullVsFile)) {
          		val word = line.split("-.\t|\t")(0)
          		if (allLemmas(word) || allLemmas(word.toLowerCase))
          			f.write(line + "\n")
          	}
	      }*/
	      Sts.vectorSpace = BowVectorSpace(opts.vectorSpace /* "resources/prob/prob.vs"*/)
	      // Index distributional phrases into Lucene repository
	      luceneDistPhrases = new Lucene(opts.phrasesFile )
	      // Index paraphrase rules into Lucene repository
	      luceneParaphrases = opts.rulesFile.split(":").map(new Lucene (_)).toList
	      //luceneParaphrases = new Lucene (opts.rulesFile)
	      Sts.pairIndex = 1;
	      Sts.text = sen1;
	      Sts.hypothesis = sen2;
	      Sts.textLemma = lemmas(0);
	      Sts.hypothesisLemma = lemmas(1);
	      println(Sts.text)
	      println(Sts.hypothesis)
	      val boxPair = boxes.map(x => Option(x.get.toString()));
          val result = runOnePair(boxPair, Sts.vectorSpace, depParser);
          println(result)
          resultOnePair = result;
 	}
    
    def runOnePair(boxPair:List[Option[String]], vectorSpace:BowVectorSpace, depParser:DepParser):Seq[Double] = {

			val compositeVectorMaker = opts.compositeVectorMaker match {
				case "mul" => MultiplicationCompositeVectorMaker();
				case "add" => SimpleCompositeVectorMaker();
				case "ngram" => NgramCompositeVectorMaker(opts.ngramN, opts.ngramAlpha);
			}
			val logicFormSource: DiscourseInterpreter[BoxerExpression] = opts.logicFormSource match {
				case "dep" => new DependencyParsedBoxerDiscourseInterpreter(depParser);
				case "box" => new PreparsedBoxerDiscourseInterpreter(boxPair, new PassthroughBoxerExpressionInterpreter());
			}			
			
			val softLogicTool: ProbabilisticTheoremProver[FolExpression] = opts.softLogicTool match {
				case "psl" => new PSLTheoremProver()
				case "none" => new NoneTheoremProver()
				case "mln" => new MLNTheoremProver()
				case "ss" => new SampleSearchTheoremProver()
			}		
			 
          val ttp =
            new TextualTheoremProver( // 1<==
              logicFormSource,
           new	EditDRSTheoremProver(
            new DoMultipleParsesTheoremProver( // 1.5<==multiple parses
	         new HandleSpecialCharProbabilisticTheoremProver( // 2<== class name is misleading. Just remove the surrounding quotes if the predicate name is quoted. 
	            		  											//This is necessary before generating inference rules, because generating inference rules searches vector space
            	new FindEventsProbabilisticTheoremProver(   //3,4<== Find event variables and prop variables. This is important to reduce domain size. 
				  new GivenNotTextProbabilisticTheoremProver(
				   new Baseline( //what baseline to run based on Sts.opts.baseline 
            	    new InferenceRuleInjectingProbabilisticTheoremProver( // 6<== Generate Inference rules on the fly + convert the other rules to FOL then add them to the inference problem. 
		                words => vectorSpace,
		                new SameLemmaHardClauseRuleWeighter(
		                  new AwithCvecspaceWithSpellingSimilarityRuleWeighter(compositeVectorMaker)),
		              new FromEntToEqvProbabilisticTheoremProver( // 6.5<== goal =  premise ^  hypothesis. This is for STS
		                 new TypeConvertingPTP( // 7<== Entry point for final modifications on Boxer's representation before converting to FOL
		                  new BoxerExpressionInterpreter[FolExpression] {
		                    def interpret(x: BoxerExpression): FolExpression = {
		                      val drt = new Boxer2DrtExpressionInterpreter().interpret( // 11<== Convert from Boxer to DRT 
		                            new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret( // 9<== replacing Merge and Alpha with DRS + remove unnecessary sub-boxes + remove unnecessary variables 
		                            																	//Removing unnecessary sub-boxes is correct as long as we are NOT doing embedded propositions.
		                              new PredicateCleaningBoxerExpressionInterpreterDecorator().interpret(x))); // 8<== replace all remaining special characters with _
		                      if (!drt.isInstanceOf[DrtApplicationExpression])//for debugging, print the DRT Boxes before convert to FOL
		                    	  LOG.trace("\n" + drt.pretty);
		                      drt.fol  // 12<== convert DRT to FOL. My question is, why move from Boxer to DRT to FOL. Why not directly to FOL ???
		                    }
		                  }, 
		                  new SetVarBindPTP( //with or without Variable Binding 		
		                   new SetPriorPTP( //
		                    new PositiveEqEliminatingProbabilisticTheoremProver( //Apply skolemized positive equalities and remove skolmeized negated equalities.
		                    new CorefProbabilisticTheoremProver( // Coreference resolution between T and H for predicates in LHS of Merge
		                     new SetGoalPTP( //
		                      new HardAssumptionAsEvidenceProbabilisticTheoremProver( // 15<== Generate evidence from premise.
		                       //new PositiveEqEliminatingProbabilisticTheoremProver( //Apply skolemized positive equalities and remove skolmeized negated equalities.		                          
		                        new AutoTypingPTP( //generate negative evidence
		                         new NoExistProbabilisticTheoremProver( //
		                        softLogicTool)))))))))))))))))) // 16<== run Alchemy or PSL

          val p = ttp.prove(Sts.text, Sts.hypothesis) //Sts.text and Sts.hypothesis are already tokenized 
          return p;
    }
    
    def run(stsFile: String, boxFile: String, lemFile: String, vsFile: String, goldSimFile: String, outputSimFile: String, allLemmas: String => Boolean, includedPairs: Int => Boolean) {
    	val pairs = readLines(stsFile).map(_.split("\t")).map { case Array(a, b) => (a, b) }
		//val lemPairs = readLines(lemFile).map(_.split("\t")).map { case Array(a, b) => (a, b) }
        Sts.vectorSpace = BowVectorSpace(vsFile)


		val boxPairs =
		FileUtils.readLines(boxFile)
          .map { case SomeRe(drsString) => Some(drsString); case "None" => None }
          .toList
          .grouped(2)
        val goldSims = FileUtils.readLines(goldSimFile).map(_.toDouble)
      
	    def probOfEnt2simScore(p: Double) = {
			if(opts.task == "sts")
				p * 5;
			else p;
		}
		// Index distributional phrases into Lucene repository
		luceneDistPhrases = new Lucene(opts.phrasesFile )
	
		// Index paraphrase rules into Lucene repository
		luceneParaphrases = opts.rulesFile.split(":").map(new Lucene (_)).toList
	
		/*
		 Sts.depParser = opts.logicFormSource match {
			case "dep" => DepParser.load();
			case "box" => null;
		}*/	 
		
		val results =
	    //for ((((((txt, hyp), boxPair), goldSim), (lemTxt, lemHyp)), i) <- (pairs zipSafe boxPairs zipSafe goldSims zipSafe lemPairs).zipWithIndex if includedPairs(i + 1)) yield {
		  for ((((((txt, hyp), boxPair), goldSim)), i) <- (pairs zipSafe boxPairs zipSafe goldSims ).zipWithIndex if includedPairs(i + 1)) yield {
	
			Sts.pairIndex = i+1;
			Sts.text = Tokenize.separateTokens(txt.toLowerCase());
			Sts.hypothesis = Tokenize.separateTokens(hyp.toLowerCase());
			Sts.textLemma = Lemmatize.lemmatizeWords(Sts.text);
			Sts.hypothesisLemma = Lemmatize.lemmatizeWords(Sts.hypothesis);
			Sts.goldStandard = goldSim;
			println("=============\n  Pair %s\n=============".format(i + 1))
			println(Sts.text)
			println(Sts.hypothesis)
			
			val p = runOnePair(boxPair, Sts.vectorSpace, depParser)
			println("Some(%s) [actual: %s, gold: %s]".format(p.mkString(":"), p.map(probOfEnt2simScore).map("%1.1f".format(_)).mkString(":"), goldSim))
			i -> (p.map(probOfEnt2simScore), goldSim)
	    }
			
      val (ps, golds) = results.map(_._2).unzip

      //print filtered DiffRules
      if (Sts.opts.printDiffRules)
    	  DiffRules.allGeneratedRules.unzip._2.foreach (r => println(r.replace("#", "")));
      
      println("[" + ps.map( r=> { r.map("%1.4f".format(_)).mkString(",") }  ).mkString(" ") + "]")
      println(golds.mkString("["," ","]"))
      FileUtils.writeUsing(outputSimFile) { f =>
        f.write(ps.mkString(" ") + "\n")
        f.write(golds.mkString(" ") + "\n")
      }

      // output some aggregate statistics
      val ps_golds: Seq[(Double, Double)] = ps.flatten.zip(golds)

      // root mean squared error
      val rmse = {
        val sqerr =
          ps_golds
            .filter(_._1 >= 0.0) // don't count ones we had errors for
            .collect { case (p, g) => { pow(p - g, 2) } }
        val mse = sqerr.sum / sqerr.size
        sqrt(mse)
      }

      // compute accuracy
      val goldOptions = golds.toSet
      val choseCorrect =
        ps_golds
          .filter(_._1 >= 0.0) // don't count ones we had errors for
          .collect {
            case (p, g) => {
              goldOptions.minBy(o => pow(o - p, 2)) == g // was the nearest option the gold value?
            }
          }
      val total = choseCorrect.length
      val right = choseCorrect.filter(_ == true).length
      val wrong = choseCorrect.filter(_ == false).length
      val accur = right.toDouble / total
      println("Accuracy: " + ("%.3f" format accur) + " (" + right + "/" + total + ")")
      println("RMSE: " + ("%.3f" format rmse))
      // todo: break down by parsing errors, mln errors, etc
      println("Errors: " + (golds.length - total))
    }
  }
}
