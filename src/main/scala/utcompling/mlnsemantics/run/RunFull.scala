package utcompling.mlnsemantics.run

import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.scalalogic.util.FileUtils
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

class RunFull(
  vecspaceFactory: (String => Boolean) => Map[String, BowVector]) {

  def run(txt: String, hyp: String): Unit = {
    run(Tokenize(txt).toList, Tokenize(hyp).toList)
  }

  def run(txt: List[String], hyp: List[String]): Unit = {
    val ttp =
      new TextualTheoremProver(
        new ModalDiscourseInterpreter(),
        new InferenceRuleInjectingProbabilisticTheoremProver(
          new WordnetImpl(),
          vecspaceFactory,
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

    println(txt.mkString(" "))
    println(hyp.mkString(" "))
    println(ttp.prove(txt, hyp))

    //    def mtpo = new ModalTheoremProver(tpo)
    //    def vtpo = new VisualizingModalTheoremProverDecorator(mtpo)
    //    def vwtpo = new VisualizingModalTheoremProverDecorator(new InferenceRuleInjectingProbabilisticTheoremProver(mtpo))
  }

}

object RunFull {

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)

    val vsf2 = (words: (String => Boolean)) => BowVectorSpace("resources/nytgiga.lem.1m.vc.f2000.m50.wInf", words)

    val runner = new RunFull(vsf2)
    runner.run("A man is riding a bicycle.", "A man is riding a bike.")

  }

}
