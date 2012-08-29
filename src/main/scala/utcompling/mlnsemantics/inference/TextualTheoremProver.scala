package utcompling.mlnsemantics.inference

import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import opennlp.scalabha.util.FileUtils.pathjoin
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.CollectionUtils._
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
import utcompling.mlnsemantics.inference.support._
import utcompling.scalalogic.discourse.DiscourseInterpreter

class TextualTheoremProver(
  discourseIterpreter: DiscourseInterpreter[BoxerExpression],
  probabilisticTheoremProver: ProbabilisticTheoremProver[BoxerExpression]) {

  def prove(text: String, hyp: String): Option[Double] = 
    prove(List(text), List(hyp))

  def prove(text: List[String], hyp: List[String]): Option[Double] = {
    val List(Some(txtEx), Some(hypEx)) = discourseIterpreter.batchInterpretMultisentence(List(text, hyp), Some(List("t", "h")), false, false)

    val IndvVar = """^(x\d*)$""".r
    val EvntVar = """^(e\d*)$""".r
    val PropVar = """^(p\d*)$""".r

    def combinePredicatesAndArgTypes(list: List[(Map[String, List[String]], Map[String, Set[String]])]): (Map[String, List[String]], Map[String, Set[String]]) = {
      val (predTypes, constTypes) = list.unzip
      val combinedPredTypes =
        predTypes.flatten.groupByKey.map {
          case (k, head :: tail) =>
            tail.foreach(t => (head zipSafe t).foreach { case (a, b) => assert(a == b, k + ": " + (head :: tail)) })
            (k, head)
        }
      val combinedConstTypes = constTypes.flatten.groupByKey.mapVals(_.flatten.toSet)
      (combinedPredTypes, combinedConstTypes)
    }

    def getPredicatesAndArgTypes(e: BoxerExpression): (Map[String, List[String]], Map[String, Set[String]]) =
      e match {
        case BoxerPred(discId, indices, variable, name, pos, sense) =>
          _getPredAndArgTypesTypes(name, List(variable))
        case BoxerNamed(discId, indices, variable, name, typ, sense) =>
          _getPredAndArgTypesTypes(name, List(variable))
        case BoxerRel(discId, indices, event, variable, name, sense) =>{
          if(name == "theme") println(e)
          _getPredAndArgTypesTypes(name, List(event, variable))
        }
        case _ => {
          e.visit(getPredicatesAndArgTypes, combinePredicatesAndArgTypes, (Map[String, List[String]](), Map[String, Set[String]]()))
        }
      }

    def _getPredAndArgTypesTypes(name: String, args: List[BoxerVariable]): (Map[String, List[String]], Map[String, Set[String]]) = {
      val (argTypes, constants) =
        args.map(_.name).foldLeft(List[String](), List[(String, String)]()) {
          case ((argTypes, constants), varName) =>
            varName match {
              case IndvVar(v) => ("indv" :: argTypes, constants)
              case EvntVar(v) => ("evnt" :: argTypes, constants)
              case PropVar(v) => ("prop" :: argTypes, constants)
              case c => ("indv" :: argTypes, "indv" -> c :: constants)
            }
        }
      val predTypes = Map(name -> argTypes.reverse)
      val constTypes = constants.toSet.groupByKey
      (predTypes, constTypes)
    }

    val (predTypes, constTypes) = combinePredicatesAndArgTypes(List(txtEx, hypEx).map(getPredicatesAndArgTypes))
    val constants =
      Map(
        "indv" -> Set("default_indv_variable"),
        "evnt" -> Set("default_evnt_variable"),
        "prop" -> Set("default_prop_variable")) ++ constTypes
    val declarations = predTypes //List("man(ind)", "mortal(ind)")
    val evidence = List() //"man(socrates)"
    val assumptions = List(HardWeightedExpression(txtEx))
    val goal = hypEx
    probabilisticTheoremProver.prove(constants, declarations, evidence, assumptions, goal)

  }

}
