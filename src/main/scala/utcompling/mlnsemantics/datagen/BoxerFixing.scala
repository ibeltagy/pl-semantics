package utcompling.mlnsemantics.datagen

import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.impl.BoxerDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.call.impl._
import opennlp.scalabha.util.FileUtils

object BoxerFixing {

  def main(args: Array[String]): Unit = {
	println("HI")	  
    val test = Iterator(
      "The tech-loaded Nasdaq composite rose 20.96 points to 1595.91, ending at its highest level for 12 months.")
      //"The man erased what the other man wrote .",
      //"John did not forget to leave .",
      //"John forgot that Bill left .",
      //"John did not forget to leave and did not manage to care .")
    val sts_short = new StsReader("resources/semantic-textual-similarity/STS.input.MSRvid.txt").flatten
    val sts_long = new StsReader("resources/semantic-textual-similarity/STS.input.MSRpar.txt").flatten

    val boxerInterpreter =
      new BoxerDiscourseInterpreter(
        candc = CandcImpl.findBinary(Some(FileUtils.pathjoin(System.getenv("HOME"), "bin/candc/bin"))),
        boxer = BoxerImpl.findBinary(Some(FileUtils.pathjoin(System.getenv("HOME"), "bin/candc/bin"))))
   var i = 291
   for (sentence <- sts_long.drop(291)) {
      println(i)
      i = i + 1
      println(sentence)
      val tokenSplit = Tokenize(sentence).mkString(" ")
      val ex = boxerInterpreter.interpret(tokenSplit)

      println(ex)
      ex.pprint()
      println
    }

  }

}
