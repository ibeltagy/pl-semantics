package utcompling.mlnsemantics.run

import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.scalalogic.util.FileUtils
import utcompling.scalalogic.util.CollectionUtils._
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

/**
 *
 *
 * sbt "run-main utcompling.mlnsemantics.run.Sts lem resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.lem""
 * sbt "run-main utcompling.mlnsemantics.run.Sts vs resources/full.vs resources/semantic-textual-similarity/STS.input.MSRvid.lem resources/semantic-textual-similarity/STS.input.MSRvid.vs"
 * sbt "run-main utcompling.mlnsemantics.run.Sts fromStsVs resources/semantic-textual-similarity/STS.input.MSRvid.txt resources/semantic-textual-similarity/STS.input.MSRvid.vs"
 */
object Sts {

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)

    args.toSeq match {
      case Seq("lem", stsFile, lemFile) =>
        val sentences = Source.fromFile(stsFile).getLines.flatMap(_.split("\t")).toVector
        val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences)
        FileUtils.writeUsing(lemFile) { f =>
          lemmatized
            .map(_.map(_.map(_._2).mkString(" ")).getOrElse("______________"))
            .grouped(2).foreach { case Seq(a, b) => f.write("%s\t%s\n".format(a, b)) }
        }

      case Seq("vs", fullVsFile, lemFile, stsVsFile) =>
        val allLemmas = Source.fromFile(lemFile).getLines.flatMap(_.split("\\s+")).toSet
        FileUtils.writeUsing(stsVsFile) { f =>
          for (line <- Source.fromFile(fullVsFile).getLines)
            if (allLemmas(line.split("\\s+")(0)))
              f.write(line + "\n")
        }

      case Seq("full", stsFile, fullVsFile) =>
        val sentences = Source.fromFile(stsFile).getLines.flatMap(_.split("\t")).toVector
        val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences)
        val allLemmas = lemmatized.flatten.flatMap(_.map(_._2)).toSet
        run(stsFile, fullVsFile, _ => true)

      case Seq("fromStsVs", stsFile, stsVsFile) =>
        run(stsFile, stsVsFile, UniversalSet())
    }

    def run(stsFile: String, vsFile: String, allLemmas: String => Boolean) {
      val ttp =
        new TextualTheoremProver(
          new ModalDiscourseInterpreter(),
          new InferenceRuleInjectingProbabilisticTheoremProver(
            new WordnetImpl(),
            BowVectorSpace(vsFile, allLemmas).filterKeys,
            new TopRuleWeighter(
              new RankingRuleWeighter(
                new VecspaceRuleWeighter(
                  new SimpleCompositeVectorMaker()))),
            new TypeConvertingPTP(
              new BoxerExpressionInterpreter[FolExpression] {
                def interpret(x: BoxerExpression): FolExpression =
                  new Boxer2DrtExpressionInterpreter().interpret(
                    new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(
                      new MergingBoxerExpressionInterpreterDecorator().interpret(
                        new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret(
                          new PredicateCleaningBoxerExpressionInterpreterDecorator().interpret(x))))).fol
              },
              new AlchemyTheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/alchemy/bin/infer")))))

      val pairs = Source.fromFile(stsFile).getLines.map(_.split("\t")).map { case Array(a, b) => (a, b) }.toVector

      for ((txt, hyp) <- pairs) {
        println(txt.mkString(" "))
        println(hyp.mkString(" "))
        println(ttp.prove(txt, hyp))
      }
    }
  }

}
