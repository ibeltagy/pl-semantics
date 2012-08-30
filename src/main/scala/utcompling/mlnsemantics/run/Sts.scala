package utcompling.mlnsemantics.run

import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.CollectionUtils._
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
 * sbt "run-main utcompling.mlnsemantics.run.Sts lem resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.lem""
 * sbt "run-main utcompling.mlnsemantics.run.Sts vs resources/full.vs resources/semantic-textual-similarity/STS.input.MSRvid.lem resources/semantic-textual-similarity/STS.input.MSRvid.vs"
 * sbt "run-main utcompling.mlnsemantics.run.Sts box resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.box"
 * sbt "run-main utcompling.mlnsemantics.run.Sts run resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.box resources/semantic-textual-similarity/STS.input.MSRvid.vs"
 */
object Sts {

  val SomeRe = """Some\((.*)\)""".r

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.DEBUG)

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

      case Seq("run", stsFile, boxFile, stsVsFile) =>
        run(stsFile, boxFile, stsVsFile, UniversalSet())

      //      case Seq("full", stsFile, fullVsFile) =>
      //        val sentences = readLines(stsFile).flatMap(_.split("\t")).toVector
      //        val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences)
      //        val allLemmas = lemmatized.flatten.flatMap(_.map(_._2)).toSet
      //        run(stsFile, fullVsFile, _ => true)
    }

    def sepTokens(a: String) = Tokenize(a).mkString(" ")

    def run(stsFile: String, boxFile: String, vsFile: String, allLemmas: String => Boolean) {
      val pairs = readLines(stsFile).map(_.split("\t")).map { case Array(a, b) => (a, b) }

      val boxPairs =
        FileUtils.readLines(boxFile)
          .map { case SomeRe(drsString) => Some(drsString); case "None" => None }
          .toList
          .grouped(2)

      for ((((txt, hyp), boxPair), i) <- (pairs zipSafe boxPairs).zipWithIndex) {
        println("\n\n========================\n  Pair %s\n========================".format(i))
        println(txt)
        println(hyp)

        val ttp =
          new TextualTheoremProver(
            new PreparsedBoxerDiscourseInterpreter(boxPair, new PassthroughBoxerExpressionInterpreter()),
            new InferenceRuleInjectingProbabilisticTheoremProver(
              new WordnetImpl(),
              words => BowVectorSpace(vsFile, x => words(x) && allLemmas(x)),
              new SameLemmaHardClauseRuleWeighter(
                new VecspaceRuleWeighter(new SimpleCompositeVectorMaker())),
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

        println(ttp.prove(sepTokens(txt), sepTokens(hyp)))
      }
    }
  }

}
