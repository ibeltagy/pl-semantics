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
import utcompling.mlnsemantics.inference.InferenceRuleInjectingProbabilisticTheoremProver
import utcompling.mlnsemantics.inference.RankingRuleWeighter
import utcompling.mlnsemantics.inference.SimpleCompositeVectorMaker
import utcompling.mlnsemantics.inference.TextualTheoremProver
import utcompling.mlnsemantics.inference.TopRuleWeighter
import utcompling.mlnsemantics.inference.TypeConvertingPTP
import utcompling.mlnsemantics.inference.VecspaceRuleWeighter
import utcompling.mlnsemantics.inference.FakeProbabilisticTheoremProver

object Baseline {

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)

    //    val txt = "an architect bought a new red car"
    //    val hyp = "a person purchased a new vehicle"

    val a = List(
      List("A man is riding a bicycle ."),
      List("A man is riding a bike ."),

      List("A stadium craze is sweeping the country ."),
      List("A stadium craze is covering the country ."), // True

      List("He left the children with the nurse ."),
      List("He entrusted the children to the nurse ."), // False without "all e all x ((leave_v_dt_1002(e) & r_with(e, x)) -> (entrust_v_dh_2002(e) & r_to(e, x)))"

      List("He left the children with the nurse ."),
      List("He entrusted the children with the nurse ."),

      List("South Korea fails to honor U.S. patents ."),
      List("South Korea does not observe U.S. patents ."), // True

      List("The U.S. is watching closely as South Korea fails to honor U.S. patents ."),
      List("South Korea does not observe U.S. patents ."), // False, stuff after "as" is treated as a nested subexpression

      List("We return to the young woman who is reading the Wrigley's wrapping paper ."),
      List("The young woman reading the Wrigley's wrapping paper is revisited ."), // False, "return to" -> "revisit" since the "to" doesn't get dropped

      //      List("After a fire extinguisher is used, it must always be returned for recharging and its use recorded ."),
      //      List("A fire extinguisher must always be sent back after using it ."), // ERROR, Boxer doesn't handle "after" right

      List("Joe Robbie could not persuade the mayor , so he built his own coliseum .", "He has used it to turn a healthy profit ."), // NOTE: "couldn't" -> "could not"
      List("Joe Robbie used a stadium to turn a sizable profit ."), // False, "persuade" doesn't produce a "theme" predicate

      List("The significance of this will be apparent when it is realized that , while the sportsman is trying to get a sight of the tiger , the tiger in all probability is trying to stalk the sportsman ."),
      List("The tiger is following the sportsman ."),

      List("Mary 's grandfather taught her necessary skills : how to tip my tea into my saucer and blow waves across it until it was cool enough to drink ; how to cut an orange in half crossways and pack a sugar lump into each half and then suck out orange-juice and sugar together ; how to walk along the crazy-paving garden path without stepping on any of the cracks or a tiger would get you ; how to butter the loaf and then clutch it to your chest and then shave off paper-thin slices ; what saint to pray to when you woke up at night and saw the devil moving behind the curtains ."),
      List("Mary was instructed some useful skills by her grandfather ."))

    //
    //
    //

    val vsf1 = (words: (String => Boolean)) => Map[String, BowVector]().withDefaultValue(new BowVector(Map("" -> 1)))
    val vsf2 = (words: (String => Boolean)) => BowVectorSpace("resources/nytgiga.lem.1m.vc.f2000.m50.wInf", words)

    val ttp =
      new TextualTheoremProver(
        new ModalDiscourseInterpreter(),
        new InferenceRuleInjectingProbabilisticTheoremProver(
          new WordnetImpl(),
          vsf1,
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
            new FakeProbabilisticTheoremProver(
              //              new TheoremProver[FolExpression, String] {
              //                def prove(assumptions: List[FolExpression], goal: Option[FolExpression] = None, verbose: Boolean = false): Option[String] = {
              //                  assumptions.map(println)
              //                  println
              //                  println(goal)
              //                  println
              //                  Option("...")
              //                }
              //              }))))

              new Prover9TheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/LADR-2009-11A/bin/prover9"), 5, false)))))

    //
    //
    //

    for (List(txt, hyp) <- a.grouped(2)) {
      println(txt.mkString(" "))
      println(hyp.mkString(" "))
      println(ttp.prove(txt, hyp))
    }

    //    def mtpo = new ModalTheoremProver(tpo)
    //    def vtpo = new VisualizingModalTheoremProverDecorator(mtpo)
    //    def vwtpo = new VisualizingModalTheoremProverDecorator(new InferenceRuleInjectingProbabilisticTheoremProver(mtpo))

  }

}
