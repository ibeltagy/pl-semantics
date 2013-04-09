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
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.mlnsemantics.datagen.CncLemmatizeCorpusMapper
import scala.io.Source
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.PassthroughBoxerExpressionInterpreter
import utcompling.scalalogic.discourse.impl.PreparsedBoxerDiscourseInterpreter
import utcompling.mlnsemantics.inference.CompositionalRuleWeighter
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.mlnsemantics.inference.DependencyParsedBoxerDiscourseInterpreter
import dhg.depparse._

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

/**
 * Input parameters
 * -vsWithPos true, false
 * -vectorMaker add, mul
 * -chopLvl rpr, rp
 * -noDup true, false
 * -scaleW true, false
 * -maxProb 0.90
 * -wThr 0.45
 * -range 1-10
 * -log OFF
 * -varBind true, false
 * -timeout integerInMilliseconds
 * -peInf true false
 * -irLvl 0 1 2
 * -logic dep box
 */



object Sts {

  //val Range(defaultRange) = "1-85,87-127,129-190,192-216,218-249,251-276,278-317,319-335,337-351,353-360,362-416,418-458,460-497,499-531,533-554,556-564,566-568,570-604,606-607,609-663,665-685,687-691,693-705,707-714,716-719,721-736,739-750"
  //val Range(defaultRange) = "28,128,532"
  /*
   * [HANDELED: noImp] 86: many FORALLS. It takes forever
   * [HANDELED: noImp] 113: one long FORALLS. It takes forever
   * [HANDELED: noImp] 250: one long FORALLS. It takes forever
   * [HANDELED: noImp] 361: one long FORALLS. It takes forever
   * [HANDELED:return 0.5] 459: Parsing failed
   * [HANDELED: cancel POS/NEG] 706: fails because of POS/NEG
   * [HANDELED:return 0.5] 941: Parsing failed
   * [HANDELED: cancel POS/NEG] 1041: fails because of POS/NEG
   * [HANDELED:replace ' ] 1046: Someone is slicing tortila's. Everything is wrong because of the " ' "
   * [HANDELED: cancel POS/NEG] 1216: fails because of POS/NEG
   * [HANDELED: cancel POS/NEG] 1239: fails because of POS/NEG
   * [HANDELED: noImp] 1367: one long FORALLS. It takes forever
   * 
   * Examples with IMP but work fine on Alchamy: 4, 478, 572, 621, 649, 665, 681, 996, 1008, 1292, 1329, 1399
   * Examples with IMP do not work on Alchamy: 86, 113, 250, 361, 1367
   */
          
  //val Range(defaultRange) = "1-85,87-112,114-249,251-360,362-705,707-1040,1042-1215,1217-1238,1240-1366,1368-1500"
  //val Range(defaultRange) = "706, 1041, 1216, 1239"
  //val Range(defaultRange) = "28,95,223,227,238"
  //Try example 597,610,679
  //val Range(defaultRange) = "597,610,679,803,825,904,905,1067,1171,1341,1399,1446"
  //val Range(defaultRange) = "1-829,831-1500"
  val Range(defaultRange) = "1-1500"
    
  var opts:Map[String, String] = Map(); //additional parameters passed from command line
  
  val SomeRe = """Some\((.*)\)""".r

  val wordnet = new WordnetImpl()

  def main(args: Array[String]) {
    val (newArgs, optPairs) =
      ("" +: args.toSeq).sliding2.foldLeft((Vector[String](), Vector[(String, String)]())) {
        case ((newArgs, opts), (a, b)) => a match {
          case _ if a.startsWith("-") => (newArgs, opts :+ (a -> b))
          case _ if b.startsWith("-") => (newArgs, opts)
          case _ => (newArgs :+ b, opts)
        }
      }

    opts = optPairs.toMap
    println("args: " + opts.mkString(", "))

    val loglevel = opts.get("-log").map(Level.toLevel).getOrElse(Level.DEBUG)

    Logger.getRootLogger.setLevel(loglevel)

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
		          val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences.slice(from, to))
		          
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
        val sentences = readLines(stsFile).flatMap(_.split("\t")).map(sepTokens).toList
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

      case Seq("run", stsFile, boxFile, stsVsFile, goldSimFile, outputSimFile) =>
        run(stsFile, boxFile, stsVsFile, goldSimFile, outputSimFile, _ => true, defaultRange.toSet)

      case Seq("run", stsFile, boxFile, stsVsFile, goldSimFile, outputSimFile, Range(range)) =>
        run(stsFile, boxFile, stsVsFile, goldSimFile, outputSimFile, UniversalSet(), range.toSet)

      //      case Seq("full", stsFile, fullVsFile) =>
      //        val sentences = readLines(stsFile).flatMap(_.split("\t")).toVector
      //        val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences)
      //        val allLemmas = lemmatized.flatten.flatMap(_.map(_._2)).toSet
      //        run(stsFile, fullVsFile, _ => true)
    }

    def sepTokens(a: String)= {
      //Tokenize(a.replace("-","" ).replace("\"", " ").replace("\'", " ").replace("‘", " ").replace("’", " ").replace("/", " ").replace("“", " ").replace("”", " ").replace(")", " ").replace("(", " ")).mkString(" ");
      //remove non-ascii characters
      //remove control 
      Tokenize("""-|'|`|‘|’|/|"|“|”|\)|\(|&|>|<|=|\$|:|\+""".r.replaceAllIn(a, " ").filterNot(  (c:Char) => ( c > 127)  )).mkString(" ");
      //Tokenize(a.replace("-","" )).mkString(" ");
    } 
    
    def run(stsFile: String, boxFile: String, vsFile: String, goldSimFile: String, outputSimFile: String, allLemmas: String => Boolean, includedPairs: Int => Boolean) {
      val pairs = readLines(stsFile).map(_.split("\t")).map { case Array(a, b) => (a, b) }

      val boxPairs =
        FileUtils.readLines(boxFile)
          .map { case SomeRe(drsString) => Some(drsString); case "None" => None }
          .toList
          .grouped(2)
      val goldSims = FileUtils.readLines(goldSimFile).map(_.toDouble)
      
      val vsFileMod = vsFile  + (opts.get("-vsWithPos") match {
		case Some(s) if (s.toBoolean == true) => ".pos";
		case _ => "";
      })

      def probOfEnt2simScore(p: Double) = p * 5

      def depParser = DepParser.load();
      val results =
        for (((((txt, hyp), boxPair), goldSim), i) <- (pairs zipSafe boxPairs zipSafe goldSims).zipWithIndex if includedPairs(i + 1)) yield {
          println("=============\n  Pair %s\n=============".format(i + 1))
          println(txt)
          println(hyp)
 
          val compositeVectorMaker = opts.get("-vectorMaker") match {
            case Some("mul") => MultiplicationCompositeVectorMaker();
            case _ => SimpleCompositeVectorMaker();
          }
          
          val logicFormSource: DiscourseInterpreter[BoxerExpression] = opts.get("-logic") match {
            case Some("dep") => new DependencyParsedBoxerDiscourseInterpreter(depParser);
            case _ => new PreparsedBoxerDiscourseInterpreter(boxPair, new PassthroughBoxerExpressionInterpreter());
          }
          
          val ttp =
            new TextualTheoremProver( //1
              logicFormSource,
              new MergeSameVarPredProbabilisticTheoremProver(
                //new FindEventsProbabilisticTheoremProver(
	              new GetPredicatesDeclarationsProbabilisticTheoremProver(
		              new InferenceRuleInjectingProbabilisticTheoremProver( //2
		                wordnet,
		                words => BowVectorSpace(vsFileMod, x => words(x) && allLemmas(x)),
		                new SameLemmaHardClauseRuleWeighter(
		                  new AwithCvecspaceWithSpillingSimilarityRuleWeighter(compositeVectorMaker)), 
		                new TypeConvertingPTP( //3
		                  new BoxerExpressionInterpreter[FolExpression] {
		                    def interpret(x: BoxerExpression): FolExpression =
		                      new Boxer2DrtExpressionInterpreter().interpret(
		                        new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(
		                          new MergingBoxerExpressionInterpreterDecorator().interpret(
		                            new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret(
		                              new PredicateCleaningBoxerExpressionInterpreterDecorator().interpret(x))))).fol
		                  },
		                	new PositiveEqEliminatingProbabilisticTheoremProver(
		                          new FromEntToEqvProbabilisticTheoremProver(   
		                    		  new ExistentialEliminatingProbabilisticTheoremProver(
		                    				  new HardAssumptionAsEvidenceProbabilisticTheoremProver(
		                    						  AlchemyTheoremProver.findBinary())))))))))

          val p = ttp.prove(sepTokens(txt), sepTokens(hyp))
          println("%s  [actual: %s, gold: %s]".format(p, probOfEnt2simScore(p.get), goldSim))
          i -> (probOfEnt2simScore(p.get), goldSim)
        }

      val (ps, golds) = results.map(_._2).unzip
      println(ps.mkString("["," ","]"))
      println(golds.mkString("["," ","]"))
    }
  }

}
