package utcompling.mlnsemantics.inference

import scala.io.Source
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.mlnsemantics.vecspace.BowVectorSpace
import utcompling.mlnsemantics.wordnet.WordnetImpl
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.UnnecessarySubboxRemovingBoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern.{ :+, +: }
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.FileUtils._
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import org.apache.log4j.Logger
import org.apache.log4j.Level

object CompMeanPaper {

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)
    
    val filename = "resources/wsj-rte/wsj0020_wordsense_pairs"

    //////////////
    // 
    //////////////

    if (args.size >= 1 && args(0) == "interpret") {
      val allSentences =
        readLines("resources/wsj-rte/wsj0020_wordsense_pairs.txt")
          .filterNot(_.startsWith("#"))
          .split("")
          .map { case original +: sentences => (original, sentences) }
          .toList
      val drss = new ModalDiscourseInterpreter().batchInterpret(allSentences.flatMap(_._2))
      writeUsing(filename + ".drs") { out =>
        val drsItr = drss.iterator
        for ((original, sentences) <- allSentences) {
          out.write(original + "\n")
          for (s <- sentences)
            out.write(s + "\n" + drsItr.next.get.toString.replaceAll("[.-]", "_") + "\n")
          out.write("\n")
        }
        assert(!drsItr.hasNext)
      }
      //    writeUsing(filename + ".drs") { out =>
      //      for ((original, sentences) <- allSentences) {
      //        out.write(original + "\n")
      //        for (s <- sentences) {
      //          println(s)
      //          val drs = new ModalDiscourseInterpreter().interpret(s)
      //          println(drs)
      //          out.write(s + "\n" + drs + "\n")
      //        }
      //        println()
      //        out.write("\n")
      //      }
      //    }

    }

    //////////////
    // 
    //////////////

    else {

      class FakeDiscourseInterpreter(filename: String) extends DiscourseInterpreter[BoxerExpression] {
        val cache =
          readLines(filename)
            .filterNot(_.startsWith("#"))
            .split("")
            .filter(_.nonEmpty)
            .flatMap(_.tail.grouped(2))
            .map(_.toTuple2)
            .zipWithIndex
            .map { case ((s, d), i) => (s, new BoxerExpressionParser(i.toString).parse(d)) }
            .toMap

        override def batchInterpretMultisentence(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[BoxerExpression]] = {
          inputs.map { case List(s) => cache.get(s) }
        }

      }

      val ttp =
        new TextualTheoremProver(
          new FakeDiscourseInterpreter(filename + ".drs"),
          new InferenceRuleInjectingProbabilisticTheoremProver(
            new WordnetImpl(),
            (words: (String => Boolean)) => BowVectorSpace("resources/nytgiga.lem.1m.vc.f2000.m50.wInf", words),
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
                        new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret(x)))).fol
              },
              new FakeProbabilisticTheoremProver(
                new Prover9TheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/LADR-2009-11A/bin/prover9"), 5, false)))))

      val premises =
        readLines("resources/wsj-rte/wsj0020_wordsense_pairs.txt").toList
          .filterNot(_.startsWith("#"))
          .split("")
          .map { case _ :: premise :: hypotheses => (premise, hypotheses.grouped(2).toList) }

      for ((p, pairs) <- premises) {
        for (List(pos, neg) <- pairs) {
          println("%-6s %-10s %s %s".format(true, ttp.prove(p, pos), p, pos))
          println("%-6s %-10s %s %s".format(false, ttp.prove(p, neg), p, neg))
        }
      }
    }
  }
}
