package utcompling.mlnsemantics.inference

import utcompling.scalalogic.drt.expression.parse.DrtLogicParser
import utcompling.mlnsemantics.modal.VisualizingModalTheoremProverDecorator
import utcompling.scalalogic.discourse.candc.parse.output.impl.Discourse
import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.mlnsemantics.modal.ModalTheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.util.FileUtils
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.mlnsemantics.wordnet.WordnetImpl
import utcompling.mlnsemantics.vecspace.BowVectorSpace
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.UnnecessarySubboxRemovingBoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.drt.expression.DrtExpression
import edu.mit.jwi.item.POS
import scala.collection.JavaConversions.asScalaBuffer

object Baseline {

  def main(args: Array[String]) {

    val txt = "an architect bought a new red car"
    val hyp = "a person purchased a new vehicle"

    val ttp =
      new TextualTheoremProver(
        new ModalDiscourseInterpreter(),
        new InferenceRuleInjectingProbabilisticTheoremProver(
          (words: (String => Boolean)) => BowVectorSpace("resources/nytgiga.lem.1m.vc.f2000.m50.wInf", words),
          new WordnetImpl(),
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

    println(ttp.prove(txt, hyp))

    //    def mtpo = new ModalTheoremProver(tpo)
    //    def vtpo = new VisualizingModalTheoremProverDecorator(mtpo)
    //    def vwtpo = new VisualizingModalTheoremProverDecorator(new InferenceRuleInjectingProbabilisticTheoremProver(mtpo))

  }

}
