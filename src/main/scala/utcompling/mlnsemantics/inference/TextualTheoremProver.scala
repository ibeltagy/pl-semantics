package utcompling.mlnsemantics.inference

import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.mlnsemantics.modal.WordnetImpl
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.util.FileUtils.pathjoin
import utcompling.scalalogic.util.FileUtils
import utcompling.scalalogic.inference.TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.UnnecessarySubboxRemovingBoxerExpressionInterpreter
import utcompling.scalalogic.drt.expression.parse.DrtLogicParser
import utcompling.scalalogic.inference.impl.Prover9TheoremProver

class TextualTheoremProver(
  ptp: AlchemyTheoremProver) {

}

object TextualTheoremProver {

  def main(args: Array[String]) {
    val atp = new AlchemyTheoremProver(pathjoin(System.getenv("HOME"), "bin/alchemy/bin/infer"))

    val natlogInterpreter = new ModalDiscourseInterpreter()
    def boxerInterpreter(x: BoxerExpression) = {
      new Boxer2DrtExpressionInterpreter().interpret(
        //new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(
        new MergingBoxerExpressionInterpreterDecorator().interpret(
          new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret(x))) //)
    }
    val wordnet = new WordnetImpl()

    def fullParse(sentence: String) =
      natlogInterpreter.process(List(List(sentence))) match {
        case List(Some((boxerEx, natlogRules))) =>
          val drs = boxerInterpreter(boxerEx)
          drs.pprint()
          drs.fol
      }

    //    val List(Some((a, natlogRules))) = natlogInterpreter.process(List(List("Socrates is a man .")))
    //    println(a)
    //    val b = new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret(a)
    //    println(b)
    //    val c = new MergingBoxerExpressionInterpreterDecorator().interpret(b)
    //    println(c)
    //    val d = new Boxer2DrtExpressionInterpreter().interpret(c)
    //    println(d)

    //    val assumptions = List("Every man bought a car .", "Socrates is a man .").map(fullParse)
    //    val goal = fullParse("A man bought a car .")
    //
    //    assumptions.foreach(println)
    //    println(goal)

    val parse = new FolLogicParser().parse(_)
    val a = parse("all x0.(man(x0) -> exists e2 x1.(car(x1) & buy(e2) & agent(e2, x0) & patient(e2, x1)))")
    val b = parse("exists x0.(socrates_org(x0) & man(x0))")
    val c = parse("exists e2 x0 x1.(man(x0) & car(x1) & buy(e2) & agent(e2, x0) & patient(e2, x1))")

    println(Prover9TheoremProver.findBinary(Some(pathjoin(System.getenv("HOME"), "bin/LADR-2009-11A/bin/")), timeout = 5).prove(List(a, b), c).isDefined)

    //    val constants = Map("ind" -> List("socrates"))
    //    val declarations = List("man(ind)", "mortal(ind)").map(parse)
    //    val evidence = List("man(socrates)").map(parse)
    //    val assumptions = List(HardWeightedExpression(parse("all x.(man(x) -> mortal(x))")))
    //    val goal = parse("mortal(socrates)")
    //    println(atp.prove(constants, declarations, evidence, assumptions, goal))

  }

}
