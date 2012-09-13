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

/**
 *
 *
 * sbt "run-main utcompling.mlnsemantics.run.Sts lem resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.lem"
 * sbt "run-main utcompling.mlnsemantics.run.Sts vs resources/full.vs resources/semantic-textual-similarity/STS.input.MSRvid.lem resources/semantic-textual-similarity/STS.input.MSRvid.vs"
 * sbt "run-main utcompling.mlnsemantics.run.Sts box resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.box"
 * sbt "run-main utcompling.mlnsemantics.run.Sts run resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.box resources/semantic-textual-similarity/STS.input.MSRvid.vs STS.gs.MSRvid.txt STS.out.MSRvid.txt"
 *
 *
 * 86 hangs
 * 128: -(x3 = x2)
 * 191: whq
 * 217: "Unrecoverable Error" in Alchemy; has factive "try"
 * 277: "Unrecoverable Error" in Alchemy; has factive "try"
 * 318: -(x3 = x0)
 * 336: -(x1 = x0)
 * 361: contains implication
 * 417: "Unrecoverable Error" in Alchemy; has factive "try"
 * 459 won't box
 * 532: -(x1 = x0)
 * 608: -(x1 = x0)
 * 664: hangs
 * 692: "Unrecoverable Error" in Alchemy; contains implication
 * 706: "Unrecoverable Error" in Alchemy; nested implicatives
 * 715: "Unrecoverable Error" in Alchemy; has factive "try"
 * 720: "Unrecoverable Error" in Alchemy; has factive "try"
 * 737: -(x1 = x0)
 * 738: -(x1 = x0)
 * 750: soft rule weight of NaN
 *
 * 1-85,87-127,129-190,192-216,218-276,278-317,319-335,337-360,362-416,418-458,460-531,533-607,609-663,665-691,693-705,707-714,716-719,721-736,739-749
 */
object Sts {

  val Range(defaultRange) = "1-85,87-127,129-190,192-216,218-276,278-317,319-335,337-360,362-416,418-458,460-531,533-607,609-663,665-691,693-705,707-714,716-719,721-736,739-749"

  val SomeRe = """Some\((.*)\)""".r

  val wordnet = new WordnetImpl()

  def main(args: Array[String]) {
    val opts = args.toSeq.grouped(2).map { case Seq(o, v) => (o, v) }.toMap

    val loglevel = opts.get("-log").map(Level.toLevel).getOrElse(Level.DEBUG)

    Logger.getRootLogger.setLevel(loglevel)

    args.toSeq match {
      case Seq("lem", stsFile, lemFile) =>
        val sentences = readLines(stsFile).flatMap(_.split("\t")).toVector
        val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences)
        FileUtils.writeUsing(lemFile) { f =>
          lemmatized
            .map(_.map(_.map(_._2).mkString(" ")).getOrElse("______parse_failed______"))
            .grouped(2).foreach { case Seq(a, b) => f.write("%s\t%s\n".format(a, b)) }
        }

      case Seq("vs", fullVsFile, lemFile, stsVsFile) =>
        val allLemmas = readLines(lemFile).flatMap(_.split("\\s+")).toSet
        FileUtils.writeUsing(stsVsFile) { f =>
          for (line <- readLines(fullVsFile))
            if (allLemmas(line.split("\\s+")(0)))
              f.write(line + "\n")
        }

      case Seq("box", stsFile, boxFile) =>
        val di = new ModalDiscourseInterpreter()
        val sentences = readLines(stsFile).flatMap(_.split("\t")).map(sepTokens).toList
        writeUsing(boxFile) { f =>
          for (x <- di.batchInterpret(sentences))
            f.write(x + "\n")
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

    def sepTokens(a: String) = Tokenize(a).mkString(" ")

    def run(stsFile: String, boxFile: String, vsFile: String, goldSimFile: String, outputSimFile: String, allLemmas: String => Boolean, includedPairs: Int => Boolean) {
      val pairs = readLines(stsFile).map(_.split("\t")).map { case Array(a, b) => (a, b) }

      val boxPairs =
        FileUtils.readLines(boxFile)
          .map { case SomeRe(drsString) => Some(drsString); case "None" => None }
          .toList
          .grouped(2)
      val goldSims = FileUtils.readLines(goldSimFile).map(_.toDouble)

      val results =
        for (((((txt, hyp), boxPair), goldSim), i) <- (pairs zipSafe boxPairs zipSafe goldSims).zipWithIndex if includedPairs(i + 1)) yield {
          println("\n\n========================\n  Pair %s\n========================".format(i + 1))
          println(txt)
          println(hyp)

          val ttp =
            new TextualTheoremProver(
              new PreparsedBoxerDiscourseInterpreter(boxPair, new PassthroughBoxerExpressionInterpreter()),
              new InferenceRuleInjectingProbabilisticTheoremProver(
                wordnet,
                words => BowVectorSpace(vsFile, x => words(x) && allLemmas(x)),
                new SameLemmaHardClauseRuleWeighter(
                  new AwithCtxCwithCtxVecspaceRuleWeighter(new SimpleCompositeVectorMaker())),
                new TypeConvertingPTP(
                  new BoxerExpressionInterpreter[FolExpression] {
                    def interpret(x: BoxerExpression): FolExpression =
                      new Boxer2DrtExpressionInterpreter().interpret(
                        new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(
                          new MergingBoxerExpressionInterpreterDecorator().interpret(
                            new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret(
                              new PredicateCleaningBoxerExpressionInterpreterDecorator().interpret(x))))).fol
                  },
                  new ExistentialEliminatingProbabilisticTheoremProver(
                    new HardAssumptionAsEvidenceProbabilisticTheoremProver(
                      new AlchemyTheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/alchemy/bin/infer")))))))

          val p = ttp.prove(sepTokens(txt), sepTokens(hyp))
          println("%s  [actual: %s, gold: %s]".format(p, p.get * 5, goldSim))
          i -> p
        }

      results foreach { r => }
    }
  }

}
