package utcompling.mlnsemantics.run

import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.Pattern.Range
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.mlnsemantics.wordnet.WordnetImpl
import utcompling.mlnsemantics.vecspace.BowVectorSpace
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import org.apache.log4j.Logger
import org.apache.log4j.Level
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.mlnsemantics.inference._
import utcompling.scalalogic.discourse.impl.PreparsedBoxerDiscourseInterpreter
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.mlnsemantics.inference.DependencyParsedBoxerDiscourseInterpreter
import dhg.depparse._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.util.Config
import utcompling.mlnsemantics.util.Lucene
import utcompling.mlnsemantics.datagen.CncLemmatizeCorpusMapper
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.drt.expression.DrtApplicationExpression

/**
 *
 */

class Sts{
  
}
object Sts {

  private val LOG = LogFactory.getLog(classOf[Sts])
  val Range(defaultRange) = "1-2000"
  val SomeRe = """Some\((.*)\)""".r

  //Global variables used by other classes
  val wordnet = new WordnetImpl()  //wordnet
  var opts:Config = null;	//command line arugments
  var text = "";			//Text
  var textLemma = "";		//Text lemmatized 
  var hypothesis = "";		//Hypothesis
  var hypothesisLemma = "";	//Hypothesis lemmatized
  var pairIndex = 0;		//Pair index
  var luceneDistPhrases:Lucene = null;	// Lucene repository of precompiled distributional phrases 
  var luceneParaphrases:Lucene = null;	// Lucene repository of precompiled paraphrases

  def main(args: Array[String]) {
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
      case Seq("lem", stsFile, lemFile) =>
        val sentences = readLines(stsFile).flatMap(_.split("\t")).toVector
        val step = 500;  //file is large. It should be partitioned before passed to the parser
        val totalSen = sentences.length;
        val itrCount = (Math.ceil (totalSen*1.0 / step)).intValue();
        
	        FileUtils.writeUsing(lemFile) { f =>
	          for (i <- 0 to itrCount-1 )
	          {
		          val from  = i * step;
		          val to = Math.min((i+1)*step, totalSen);
					println (from + ", " +  to);
					 println (sentences.slice(from, to));
		          val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences.slice(from, to))
		          println(lemmatized);
		          lemmatized
		            .map(_.map(_.map(_._2).mkString(" ")).getOrElse("______parse_failed______"))
		            .grouped(2).foreach { case Seq(a, b) => f.write("%s\t%s\n".format(a, b)) }
	          }
	        }
        

      case Seq("vs", fullVsFile, lemFile, stsVsFile) =>
        val allLemmas = readLines(lemFile).flatMap(_.split("\\s+")).toSet
        FileUtils.writeUsing(stsVsFile) { f =>
          for (line <- readLines(fullVsFile))
            if (allLemmas(line.split("\\s+|-")(0)))
            {
              val modifiedLine = """-j\t|-r\t""".r.replaceAllIn(line, "-a\t");
              f.write(modifiedLine + "\n")
            }
        }

      case Seq("box", stsFile, boxFile) =>
        val di = new ModalDiscourseInterpreter()
        val sentences = readLines(stsFile).flatMap(_.split("\t")).map(Tokenize.separateTokens).toList
        val step = 400; //file is large. It should be partitioned before passed to the parser
        val totalSen = sentences.length;
        val itrCount = (Math.ceil (totalSen*1.0 / step)).intValue();   
        writeUsing(boxFile) { f =>
          for (i <- 0 to itrCount-1 )
          {
        	  val from  = i * step;
        	  val to = Math.min((i+1)*step, totalSen);
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
    
    def run(stsFile: String, boxFile: String, lemFile: String, vsFile: String, goldSimFile: String, outputSimFile: String, allLemmas: String => Boolean, includedPairs: Int => Boolean) {
    	val pairs = readLines(stsFile).map(_.split("\t")).map { case Array(a, b) => (a, b) }
		val lemPairs = readLines(lemFile).map(_.split("\t")).map { case Array(a, b) => (a, b) }

		val boxPairs =
		FileUtils.readLines(boxFile)
          .map { case SomeRe(drsString) => Some(drsString); case "None" => None }
          .toList
          .grouped(2)
        val goldSims = FileUtils.readLines(goldSimFile).map(_.toDouble)
      
      	val vsFileMod = vsFile + ( if(opts.vectorspaceFormatWithPOS) ".pos" else "" );

	    def probOfEnt2simScore(p: Double) = {
			if(opts.task == "sts")
				p * 5;
			else p;
		}
		// Index distributional phrases into Lucene repository
		luceneDistPhrases = new Lucene(opts.phrasesFile )
	
		// Index paraphrase rules into Lucene repository
		luceneParaphrases = new Lucene (opts.rulesFile)
	
		def depParser = DepParser.load();
		
		val results =
	    for ((((((txt, hyp), boxPair), goldSim), (lemTxt, lemHyp)), i) <- (pairs zipSafe boxPairs zipSafe goldSims zipSafe lemPairs).zipWithIndex if includedPairs(i + 1)) yield {
	
			Sts.pairIndex = i+1;
			Sts.text = txt;
			Sts.hypothesis = hyp;
			Sts.textLemma = lemTxt;
			Sts.hypothesisLemma = lemHyp;
			
			println("=============\n  Pair %s\n=============".format(i + 1))
			println(txt)
			println(hyp)
		
			val compositeVectorMaker = opts.compositeVectorMaker match {
				case "mul" => MultiplicationCompositeVectorMaker();
				case "add" => SimpleCompositeVectorMaker();
			}			  
			val logicFormSource: DiscourseInterpreter[BoxerExpression] = opts.logicFormSource match {
				case "dep" => new DependencyParsedBoxerDiscourseInterpreter(depParser);
				case "box" => new PreparsedBoxerDiscourseInterpreter(boxPair, new PassthroughBoxerExpressionInterpreter());
			}			
			val softLogicTool: ProbabilisticTheoremProver[FolExpression] = opts.softLogicTool match {
				case "psl" => new PSLTheoremProver()
				case "none" => new NoneTheoremProver()
				case "mln" => AlchemyTheoremProver.findBinary(wordnet);
			}		
			 
          val ttp =
            new TextualTheoremProver( // 1<==
              logicFormSource,
	     new DoMultipleParsesTheoremProver( // 1.5<==multiple parses
	         new HandleSpecialCharProbabilisticTheoremProver( // 2<== class name is misleading. Just remove the surrounding quotes if the predicate name is quoted. 
	            		  											//This is necessary before generating inference rules, because generating inference rules searches vector space
            	new FindEventsProbabilisticTheoremProver(   //3,4<== Find event variables and prop variables. This is important to reduce domain size. 
            	    new InferenceRuleInjectingProbabilisticTheoremProver( // 6<== Generate Inference rules on the fly + convert the other rules to FOL then add them to the inference problem. 
		                words => BowVectorSpace(vsFileMod, x => words(x) && allLemmas(x)),
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
		                    new SetGoalPTP( //
		                	 new HardAssumptionAsEvidenceProbabilisticTheoremProver( // 15<== Generate evidence from premise. 
		                    		softLogicTool))))))))))) // 16<== run Alchemy or PSL

          val p = ttp.prove(Tokenize(txt).mkString(" "), Tokenize(hyp).mkString(" "))
          println("Some(%.2f) [actual: %.2f, gold: %s]".format(p.get, probOfEnt2simScore(p.get), goldSim))
          i -> (probOfEnt2simScore(p.get), goldSim)
        }

      val (ps, golds) = results.map(_._2).unzip
      println(ps.mkString("["," ","]"))
      println(golds.mkString("["," ","]"))
	  FileUtils.writeUsing(outputSimFile) { f =>
           f.write(ps.mkString(" ") + "\n")
		   f.write(golds.mkString(" ") + "\n")
        }
    }
  }
}