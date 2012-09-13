package utcompling.mlnsemantics.run

import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.CollectionUtil._
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.scalalogic.drt.expression.DrtExpression

object BoxerCli {

  def sepTokens(a: String) = Tokenize(a).mkString(" ")
  val di = new ModalDiscourseInterpreter()

  def clean(boxerEx: BoxerExpression) = {
    new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(
      new MergingBoxerExpressionInterpreterDecorator().interpret(
        new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret(
          new PredicateCleaningBoxerExpressionInterpreterDecorator().interpret(boxerEx))))
  }

  def toDrs(box: BoxerExpression) = {
    new Boxer2DrtExpressionInterpreter().interpret(box)
  }

  def main(args: Array[String]) {
    val opts = args.toSeq.grouped(2).map { case Seq(o, v) => (o, v) }.toMap

    val draw = opts.getOrElse("-draw", "true").toBoolean
    val boxer = opts.getOrElse("-boxer", "true").toBoolean
    val drt = opts.get("-drs").orElse(opts.get("-drt")).map(_.toBoolean).getOrElse(true)
    val fol = opts.getOrElse("-fol", "true").toBoolean

    assert(opts.contains("-f") || opts.contains("-s"), "specify '-f FILENAME' or '-s SENTENCE'")
    assert(!opts.contains("-f") || !opts.contains("-s"), "cannot specify -f and -s")

    def out(boxerOpt: Option[BoxerExpression]) = {
      boxerOpt match {
        case Some(boxerOutput) =>
          val cleaned = clean(boxerOutput)
          val drs = toDrs(cleaned)

          if (draw) println(drs.pretty.split("\n").map("""\\ """ + _).mkString("\n"))
          if (boxer) println(cleaned)
          if (drt) println(drs)
          if (fol) println(drs.fol)

        case None =>
          println("FAILED TO PARSE")
      }
    }

    opts.get("-f").foreach {
      filename =>
        val sentences = FileUtils.readLines(filename).map(sepTokens).toList
        for ((sentence, boxerOpt) <- sentences zipSafe di.batchInterpret(sentences)) {
          println(sentence)
          out(boxerOpt)
        }
    }

    opts.get("-s").foreach {
      sentence =>
        println(sentence)
        val List(boxerOpt) = di.batchInterpret(List(sepTokens(sentence)))
        out(boxerOpt)
    }

  }

}
