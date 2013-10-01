package utcompling.mlnsemantics.run

import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
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
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.UnnecessarySubboxRemovingBoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import org.apache.log4j.Logger
import org.apache.log4j.Level
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.PredicateCleaningBoxerExpressionInterpreterDecorator
import utcompling.mlnsemantics.inference._
import utcompling.mlnsemantics.datagen.CncLemmatizeCorpusMapper
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.PassthroughBoxerExpressionInterpreter
import utcompling.scalalogic.discourse.impl.PreparsedBoxerDiscourseInterpreter
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.mlnsemantics.inference.DependencyParsedBoxerDiscourseInterpreter
import dhg.depparse._
import utcompling.mlnsemantics.vecspace.DistRules
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.mlnsemantics.datagen.SimpleTokenizer
import utcompling.mlnsemantics.util.Config
import utcompling.mlnsemantics.util.Lucene

/**
 *
 *
 * sbt "run-main utcompling.mlnsemantics.run.Sts lem resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.lem"
 * sbt "run-main utcompling.mlnsemantics.run.Sts vs resources/full.vs resources/semantic-textual-similarity/STS.input.MSRvid.lem resources/semantic-textual-similarity/STS.input.MSRvid.vs"
 * sbt "run-main utcompling.mlnsemantics.run.Sts box resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.box"
 * sbt "run-main utcompling.mlnsemantics.run.Sts run resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.box resources/semantic-textual-similarity/STS.input.MSRvid.vs STS.gs.MSRvid.txt STS.out.MSRvid.txt"
 *
 * 
 * 86: hangs
 * 128: -(x3 = x2)
 * 191: whq
 * 217: "Unrecoverable Error" in Alchemy; has factive "try"
 * 250: sentensec has complex forall and exist, generating evidences, additional assumption, entailment, all of them are wrong
 * 277: "Unrecoverable Error" in Alchemy; has factive "try"
 * 318: -(x3 = x0)
 * 336: -(x1 = x0)
 * 361: contains implication
 * 417: "Unrecoverable Error" in Alchemy; has factive "try"
 * 459 won't box
 * 532: -(x1 = x0)
 * 555: (!((x4 = x2)) => entailment("entailed"))
 * 565: (!((x4 = x2)) => entailment("entailed"))
 * 569: (!((x4 = x2)) => entailment("entailed"))
 * 608: -(x1 = x0)
 * 664: hangs
 * 692: "Unrecoverable Error" in Alchemy; contains implication
 * 706: "Unrecoverable Error" in Alchemy; nested implicatives
 * 715: "Unrecoverable Error" in Alchemy; has factive "try"
 * 720: "Unrecoverable Error" in Alchemy; has factive "try"
 * 737: -(x1 = x0)
 * 738: -(x1 = x0)
 * (FIXED)  750: soft rule weight of NaN
 * 
 * 352: 13 mins, 58.53 secs;
 * 498: 12 mins, 44.06 secs;
 * 605: 7 mins, 34.31 secs;
 * 686: 15 mins, 36.24 secs;
 *
 * 1-85,87-127,129-190,192-216,218-276,278-317,319-335,337-360,362-416,418-458,460-531,533-554,556-564,566-568,570-607,609-663,665-691,693-705,707-714,716-719,721-736,739-75
 */

class Sts{
  
}
object Sts {

  //val Range(defaultRange) = "1-85,87-127,129-190,192-216,218-249,251-276,278-317,319-335,337-351,353-360,362-416,418-458,460-497,499-531,533-554,556-564,566-568,570-604,606-607,609-663,665-685,687-691,693-705,707-714,716-719,721-736,739-750"
  //val Range(defaultRange) = "28,128,532"
  /*
   * [HANDELED: noImp] 86: many FORALLS. It takes forever
   * [HANDELED: noImp] 113: one long FORALLS. It takes forever
   * [HANDELED: noImp] 250: one long FORALLS. It takes forever
   * [HANDELED: noImp] 361: one long FORALLS. `It takes forever
   * [HANDELED:return 0.5] 459: Parsing failed
   * [HANDELED: cancel POS/NEG] 706: fails because of POS/NEG
   * [HANDELED:return 0.5] 941: Parsing failed
   * [HANDELED: cancel POS/NEG] 1041: fails because of POS/NEG
   * [HANDELED:replace ' ] 1046: Someone is slicing tortila's. Everything is wrong because of the " ' "
   * [HANDELED: cancel POS/NEG] 1216: fails because of POS/NEG
   * [HANDELED: cancel POS/NEG] 1239: fails because of POS/NEG
   * [HANDELED: noImp] 1367: one long FORALLS. It takes forever
   * 
   * Examples with IMP but work fine on Alchemy: 4, 478, 572, 621, 649, 665, 681, 996, 1008, 1292, 1329, 1399
   * Examples with IMP do not work on Alchemy: 86, 113, 250, 361, 1367
   */
          
  //val Range(defaultRange) = "1-85,87-112,114-249,251-360,362-705,707-1040,1042-1215,1217-1238,1240-1366,1368-1500"
  //val Range(defaultRange) = "706, 1041, 1216, 1239"
  //val Range(defaultRange) = "28,95,223,227,238"
  //Try example 597,610,679
  //val Range(defaultRange) = "597,610,679,803,825,904,905,1067,1171,1341,1399,1446"
  //val Range(defaultRange) = "1-829,831-1500"

  private val LOG = LogFactory.getLog(classOf[Sts])

  val Range(defaultRange) = "1-2000"
    
  //var opts:Map[String, String] = Map(); //additional parameters passed from command line
  var opts:Config = null;
  
  val SomeRe = """Some\((.*)\)""".r

  val wordnet = new WordnetImpl()
  
  var pairIndex = 0;

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

      //      case Seq("full", stsFile, fullVsFile) =>
      //        val sentences = readLines(stsFile).flatMap(_.split("\t")).toVector
      //        val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences)
      //        val allLemmas = lemmatized.flatten.flatMap(_.map(_._2)).toSet
      //        run(stsFile, fullVsFile, _ => true)
    }
    
    def run(stsFile: String, boxFile: String, lemFile: String, vsFile: String, goldSimFile: String, outputSimFile: String, allLemmas: String => Boolean, includedPairs: Int => Boolean) {
		//val pairs = readLines(stsFile, "ISO-8859-1").map(_.split("\t")).map { case Array(a, b) => (a, b) }
		//val lemPairs = readLines(lemFile, "ISO-8859-1").map(_.split("\t")).map { case Array(a, b) => (a, b) }
    	val pairs = readLines(stsFile).map(_.split("\t")).map { case Array(a, b) => (a, b) }
		val lemPairs = readLines(lemFile).map(_.split("\t")).map { case Array(a, b) => (a, b) }

		val boxPairs =
        //FileUtils.readLines(boxFile, "ISO-8859-1")
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
		// Index phrases into Lucene repository
		val luceneForPhrases = new Lucene(opts.phrasesFile )
	
		// Index paraphrase rules into Lucene repository
		val lucene = new Lucene (opts.rulesFile)
	
		def depParser = DepParser.load();
		val results =
	    for ((((((txt, hyp), boxPair), goldSim), (lemTxt, lemHyp)), i) <- (pairs zipSafe boxPairs zipSafe goldSims zipSafe lemPairs).zipWithIndex if includedPairs(i + 1)) yield {
	
			Sts.pairIndex = i+1;
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
				case "mln" => AlchemyTheoremProver.findBinary(wordnet);
			}		
			// Search phrases in Text-Hypothesis pair
			val ignoredTokens = List("an", "the", "be", "is", "are", "to", "in", "on", "at", "of", "for")
			
			//=========================The following code should be moved somewhere else. It is related to Lucene========================== 
			// Search phrases in Text
			val txtQuery = SimpleTokenizer(txt + " " + lemTxt)
				.filter(token => token.length > 1 && !ignoredTokens.contains(token))
				.distinct
				.mkString(" ")
			
			val txtPhrases = luceneForPhrases.read(txtQuery)
				.filter { phrase =>

				val Array(id, content) = phrase.split("\t")
				val str = " " + content + " "
	
				val simpleTxt = (" " + SimpleTokenizer(txt).mkString(" ") + " ")
				val simpleLemTxt = (" " + SimpleTokenizer(lemTxt).mkString(" ") + " ")
	
				val cond = simpleTxt.contains(str) || simpleLemTxt.contains(str)
	
				val extraStr = " some" + str
				val extraCond = !simpleTxt.contains(extraStr) && !simpleLemTxt.contains(extraStr)
	
				cond && extraCond
			}.toList

			// Search phrases in Hypothesis
			val hypQuery = SimpleTokenizer(hyp + " " + lemHyp)
				.filter(token => token.length > 1 && !ignoredTokens.contains(token))
				.distinct
				.mkString(" ")
			
			val hypPhrases = luceneForPhrases.read(hypQuery)
				.filter { phrase =>
		
					val Array(id, content) = phrase.split("\t")
					val str = " " + content + " "
		
					val simpleHyp =  (" " + SimpleTokenizer(hyp).mkString(" ") + " ")
					val simpleLemHyp = (" " + SimpleTokenizer(lemHyp).mkString(" ") + " ")
		
					val cond = simpleHyp.contains(str) || simpleLemHyp.contains(str)
		
					val extraStr = " some" + str
					val extraCond = !simpleHyp.contains(extraStr) && !simpleLemHyp.contains(extraStr)
		
					cond && extraCond
				}.toList

			// Compute similaritis between phrases and generate corresponding rules in the following format: 
			// <id> TAB <text_phrase> TAB <hypo_phrase> TAB <sim_score>
			val distRules = if(opts.phraseVecsFile != "")
								DistRules(opts.phraseVecsFile, txtPhrases, hypPhrases);
							else List[String]();
         LOG.trace ("Distributional rules: ");
         distRules.foreach(rule => LOG.trace(rule))

	

			// Search rules in Text-Hypothesis pair
			val start = System.nanoTime
			val query = SimpleTokenizer(hyp + " " + lemHyp)
				.filter(token => token.length > 1 && !ignoredTokens.contains(token))
				.distinct
				.mkString(" ")
		
			val returnedRules = lucene.read(query)
			val end = System.nanoTime
			println("Searching time: " + (end - start) / 1e9 + " s")       
			println("# returned rules: " + returnedRules.size)
		
			val filterStart = System.nanoTime
			val paraphraseRules = returnedRules
				.filter { rule =>
		
					val Array(id, left, right, score) = rule.split("\t")
					val lhs = (" " + left + " ").replaceAll(" (a|an|the) ", " ")
					val rhs = (" " + right + " ").replaceAll(" (a|an|the) ", " ")
		
					val simpleTxt = (" " + SimpleTokenizer(txt).mkString(" ") + " ").replaceAll(" (a|an|the) ", " ")
					val simpleHyp =  (" " + SimpleTokenizer(hyp).mkString(" ") + " ").replaceAll(" (a|an|the) ", " ")
		
					val simpleLemTxt = (" " + SimpleTokenizer(lemTxt).mkString(" ") + " ").replaceAll(" (a|an|the) ", " ")
					val simpleLemHyp = (" " + SimpleTokenizer(lemHyp).mkString(" ") + " ").replaceAll(" (a|an|the) ", " ")
		
					(simpleTxt.contains(lhs) || simpleLemTxt.contains(lhs) ) && 
						(simpleHyp.contains(rhs) || simpleLemHyp.contains(rhs) )
				}.toList
		
			val filterEnd = System.nanoTime
			println("Filtering time: " + (filterEnd - filterStart) / 1e9 + " s")
			LOG.trace ("Filtered rules: ");
			paraphraseRules.foreach(rule => LOG.trace(rule))
			
		 //=========================================END of code that should be moved away===================================================

          val ttp =
            new TextualTheoremProver( // 1<==
              logicFormSource,
	     new DoMultipleParsesTheoremProver( // 2<==
	      0, // pairId
            //new MergeSameVarPredProbabilisticTheoremProver(  //This is completely wrong. Do not merge vars of different parsee
	      	 //new PositiveEqEliminatingProbabilisticTheoremProver(  //Replacing equalities with variable renaming is wrong. For example, if the equality is negated, or in the RHS of an implication	      
                //new FindEventsProbabilisticTheoremProver(   //3<== Find event variables and prop variables. This is important to reduce domain size. 
	      													   //However, InferenceRuleInjectingProbabilisticTheoremProver breaks because of it. 
	      														//Fix InferenceRuleInjectingProbabilisticTheoremProver before uncomment this
	      														//Anyway, variables types is not supported in PSL
	              new HandleSpecialCharProbabilisticTheoremProver( // 4<== class name is misleading. Just remove the surrounding quotes if the predicate name is quoted. 
	            		  											//This is necessary before generating inference rules, because generating inference rules searches vector space 
	                new GetPredicatesDeclarationsProbabilisticTheoremProver(  // 5<==Generate predicates declarations. I believe this should be moved closer to the inference   
		              new InferenceRuleInjectingProbabilisticTheoremProver( // 6<== Generate Inference rules on the fly + convert the other rules to FOL then add them to the inference problem.
		            		  												// This file need significant rewriting 
		                wordnet,
		                words => BowVectorSpace(vsFileMod, x => words(x) && allLemmas(x)),
		                new SameLemmaHardClauseRuleWeighter(
		                  new AwithCvecspaceWithSpellingSimilarityRuleWeighter(compositeVectorMaker)), 
						distRules ++ paraphraseRules,
						opts.distWeight,
						opts.rulesWeight,
		                new TypeConvertingPTP( // 7<== Entry point for final modifications on Boxer's representation before converting to FOL
		                  new BoxerExpressionInterpreter[FolExpression] {
		                    def interpret(x: BoxerExpression): FolExpression =
		                      new Boxer2DrtExpressionInterpreter().interpret( // 11<== Convert from Boxer to DRT 
		                        //new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(  //empty 
		                          new MergingBoxerExpressionInterpreterDecorator().interpret( // 10<== merging some unnecessery boxes 
		                            new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret( // 9<== I could not understand this 
		                              new PredicateCleaningBoxerExpressionInterpreterDecorator().interpret( // 8<== replace all remaining special characters with _
		                                  x)))).fol  // 12<== conver DRT to FOL. My question is, why move from Boxer to DRT to FOL. Why not directly to FOL ???
		                  },
		                      new FromEntToEqvProbabilisticTheoremProver( //5: ANDing goals  
		                    		  new ExistentialEliminatingProbabilisticTheoremProver(
		                    				  new HardAssumptionAsEvidenceProbabilisticTheoremProver(//6: generate evidences
		                    						  softLogicTool))))))))) //Alchemy or PSL

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
