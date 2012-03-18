package utcompling.mlnsemantics.inference

import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.util.FileUtils.pathjoin
import utcompling.scalalogic.util.FileUtils
import utcompling.scalalogic.util.CollectionUtils._
import utcompling.scalalogic.inference.TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.UnnecessarySubboxRemovingBoxerExpressionInterpreter
import utcompling.scalalogic.drt.expression.parse.DrtLogicParser
import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.FolApplicationExpression
import utcompling.scalalogic.fol.expression.FolVariableExpression
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SetBuilder
import utcompling.scalalogic.fol.expression.FolAtom
import org.apache.log4j.Logger
import org.apache.log4j.Level
import utcompling.mlnsemantics.wordnet.WordnetImpl

class TextualTheoremProver(
  ptp: AlchemyTheoremProver) {

}

object TextualTheoremProver {

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.DEBUG)

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

    //    val constants = Map("ind" -> Set("socrates"))
    //    val declarations = List("man(ind)", "mortal(ind)").map(parse)
    //    val evidence = List("man(socrates)").map(parse)
    //    val assumptions = List(HardWeightedExpression(parse("all x.(man(x) -> mortal(x))")))
    //    val goal = parse("mortal(socrates)")
    //    println(atp.prove(constants, declarations, evidence, assumptions, goal))

    val IndvVar = """^(x\d*)$""".r
    val EvntVar = """^(e\d*)$""".r
    val PropVar = """^(p\d*)$""".r

    def combinePredicatesAndArgTypes(list: List[(Map[FolExpression, Seq[String]], Map[String, Set[String]])]): (Map[FolExpression, Seq[String]], Map[String, Set[String]]) = {
      val (predTypes, constTypes) = list.unzip
      val combinedPredTypes =
        predTypes.flatten.groupByKey.mapValuesStrict {
          case head :: tail =>
            tail.foreach(t => assert(head == t))
            head
        }
      val combinedConstTypes = constTypes.flatten.groupByKey.mapValuesStrict(_.flatten.toSet)
      (combinedPredTypes, combinedConstTypes)
    }

    def getPredicatesAndArgTypes(e: FolExpression): (Map[FolExpression, Seq[String]], Map[String, Set[String]]) =
      e match {
        case e @ FolAtom(_, args @ _*) =>
          val constants = new ListBuffer[(String, String)]
          val argTypes =
            args.map(_.name).map {
              case IndvVar(v) => "indv"
              case EvntVar(v) => "evnt"
              case PropVar(v) => "prop"
              case c => constants += "indv" -> c; "indv"
            }
          val predTypes = Map(e.pred -> argTypes)
          val constTypes = constants.toSet.groupByKey
          (predTypes, constTypes)
        case _ =>
          e.visit(getPredicatesAndArgTypes, combinePredicatesAndArgTypes)
      }

    val (predTypes, constTypes) = combinePredicatesAndArgTypes(List(a, b, c).map(getPredicatesAndArgTypes))
    val constants =
      Map(
        "indv" -> Set("default_indv_variable"),
        "evnt" -> Set("default_evnt_variable"),
        "prop" -> Set("default_prop_variable")) ++ constTypes
    val declarations = predTypes.toList.map { case (pred, args) => pred(args.map(n => FolVariableExpression(Variable(n))): _*) } //List("man(ind)", "mortal(ind)")
    val evidence = List() //"man(socrates)"
    val assumptions = List(HardWeightedExpression(a), HardWeightedExpression(b))
    val goal = c
    println(atp.prove(constants, declarations, evidence, assumptions, goal))

  }

}
