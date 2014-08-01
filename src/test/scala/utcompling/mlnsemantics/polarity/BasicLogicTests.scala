package utcompling.mlnsemantics.polarity

import utcompling.scalalogic.inference.TheoremProver
import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.drt.expression._
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.scalalogic.discourse.impl.BoxerDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.call.impl._
import opennlp.scalabha.util.FileUtils
import org.junit.Test

class BasicLogicTests {
  def test() {

    val cr = new CaseRunner(
      discourseInterpreter = new BoxerDiscourseInterpreter[DrtExpression](
        candc = new CandcImpl(),
        boxer = new BoxerImpl()),
      prover = new Prover9TheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/LADR-2009-11A/bin/prover9"), 5, false))

    println("quantification")

    cr.run("U+ U+ down", List("Fido is a dog .", "Every dog walks ."), "Every dog walks .", Some(true))
    cr.run("U+ U+ down", List("Fido is a dog .", "Every dog walks ."), "Some dogs do not walk .", Some(false))
    cr.run("U+ U+ down", List("Fido is a dog .", "Every dog walks ."), "A dog walks .", Some(true))
    cr.run("U+ U+ down", List("Fido is a dog .", "Every dog walks ."), "No dogs walk .", Some(false))

    cr.run("U+ U+ down", List("Fido is a dog .", "Some dogs do not walk ."), "Every dog walks .", Some(false))
    cr.run("U+ U+ down", List("Fido is a dog .", "Some dogs do not walk ."), "Some dogs do not walk .", Some(true))
    cr.run("U+ U+ down", List("Fido is a dog .", "Some dogs do not walk ."), "A dog walks .", None)
    cr.run("U+ U+ down", List("Fido is a dog .", "Some dogs do not walk ."), "No dogs walk .", None)

    cr.run("U+ U+ down", List("Fido is a dog .", "A dog walks ."), "Every dog walks .", None)
    cr.run("U+ U+ down", List("Fido is a dog .", "A dog walks ."), "Some dogs do not walk .", None)
    cr.run("U+ U+ down", List("Fido is a dog .", "A dog walks ."), "A dog walks .", Some(true))
    cr.run("U+ U+ down", List("Fido is a dog .", "A dog walks ."), "No dogs walk .", Some(false))

    cr.run("U+ U+ down", List("Fido is a dog .", "No dogs walk ."), "Every dog walks .", Some(false))
    cr.run("U+ U+ down", List("Fido is a dog .", "No dogs walk ."), "Some dogs do not walk .", Some(true))
    cr.run("U+ U+ down", List("Fido is a dog .", "No dogs walk ."), "A dog walks .", Some(false))
    cr.run("U+ U+ down", List("Fido is a dog .", "No dogs walk ."), "No dogs walk .", Some(true))

    //        println("modifiers and quantification")
    //
    //    cr.run("U+ U+ down", List("Fido is a big dog .", "Every dog walks ."), "Every big dog walks .", true)
    //    cr.run("U+ U- down", List("Fido is a big dog .", "Every dog walks ."), "Some big dogs do not walk .", false)
    //    cr.run("U+ E+ down", List("Fido is a big dog .", "Every dog walks ."), "A big dog walks .", true)
    //    cr.run("U+ E- down", List("Fido is a big dog .", "Every dog walks ."), "No big dogs walk .", false)
    //    
    //    cr.run("U- U+ down", List("Fido is a big dog .", "Some dogs do not walk ."), "Every big dog walks .", false)
    //    cr.run("U- U- down", List("Fido is a big dog .", "Some dogs do not walk ."), "Some big dogs do not walk .", false)
    //    cr.run("U- E+ down", List("Fido is a big dog .", "Some dogs do not walk ."), "A big dog walks .", false)
    //    cr.run("U- E- down", List("Fido is a big dog .", "Some dogs do not walk ."), "No big dogs walk .", false)
    //
    //    cr.run("E+ U+ down", List("Fido is a big dog .", "A dog walks ."), "Every big dog walks .", false)
    //    cr.run("E+ U- down", List("Fido is a big dog .", "A dog walks ."), "Some big dogs do not walk .", false)
    //    cr.run("E+ E+ down", List("Fido is a big dog .", "A dog walks ."), "A big dog walks .", false)
    //    cr.run("E+ E- down", List("Fido is a big dog .", "A dog walks ."), "No big dogs walk .", false)
    //
    //    cr.run("E- U+ down", List("Fido is a big dog .", "No dogs walk ."), "Every big dog walks .", false)
    //    cr.run("E- U- down", List("Fido is a big dog .", "No dogs walk ."), "Some big dogs do not walk .", true)
    //    cr.run("E- E+ down", List("Fido is a big dog .", "No dogs walk ."), "A big dog walks .", false)
    //    cr.run("E- E- down", List("Fido is a big dog .", "No dogs walk ."), "No big dogs walk .", true)
    //
    //
    //    cr.run("U+ U+ up",   List("Fido is a big dog .", "Every big dog walks ."), "Every dog walks .", false)
    //    cr.run("U+ U- up",   List("Fido is a big dog .", "Every big dog walks ."), "Some dogs do not walk .", false)
    //    cr.run("U+ E+ up",   List("Fido is a big dog .", "Every big dog walks ."), "A dog walks .", true)
    //    cr.run("U+ E- up",   List("Fido is a big dog .", "Every big dog walks ."), "No dogs walk .", false)
    //    
    //    cr.run("U- U+ up",   List("Fido is a big dog .", "Some big dogs do not walk ."), "Every dog walks .", false)
    //    cr.run("U- U- up",   List("Fido is a big dog .", "Some big dogs do not walk ."), "Some dogs do not walk .", true)
    //    cr.run("U- E+ up",   List("Fido is a big dog .", "Some big dogs do not walk ."), "A dog walks .", false)
    //    cr.run("U- E- up",   List("Fido is a big dog .", "Some big dogs do not walk ."), "No dogs walk .", false)
    //
    //    cr.run("E+ U+ up",   List("Fido is a big dog .", "A big dog walks ."), "Every dog walks .", false)
    //    cr.run("E+ U- up",   List("Fido is a big dog .", "A big dog walks ."), "Some dogs do not walk .", false)
    //    cr.run("E+ E+ up",   List("Fido is a big dog .", "A big dog walks ."), "A dog walks .", true)
    //    cr.run("E+ E- up",   List("Fido is a big dog .", "A big dog walks ."), "No dogs walk .", false)
    //
    //    cr.run("E- U+ up",   List("Fido is a big dog .", "No big dogs walk ."), "Every dog walks .", false)
    //    cr.run("E- U- up",   List("Fido is a big dog .", "No big dogs walk ."), "Some dogs do not walk .", true)
    //    cr.run("E- E+ up",   List("Fido is a big dog .", "No big dogs walk ."), "A dog walks .", false)
    //    cr.run("E- E- up",   List("Fido is a big dog .", "No big dogs walk ."), "No dogs walk .", false)
    //
    //        println("hypernym / hyponym with simple quantification")
    //
    //    cr.run("U+ U+ down", List("Fido is a poodle .", "Every dog walks ."), "Every poodle walks .", true)
    //    cr.run("U+ U- down", List("Fido is a poodle .", "Every dog walks ."), "Some poodles do not walk .", false)
    //    cr.run("U+ E+ down", List("Fido is a poodle .", "Every dog walks ."), "A poodle walks .", true)
    //    cr.run("U+ E- down", List("Fido is a poodle .", "Every dog walks ."), "No poodles walk .", false)
    //    
    //    cr.run("U- U+ down", List("Fido is a poodle .", "Some dogs do not walk ."), "Every poodle walks .", false)
    //    cr.run("U- U- down", List("Fido is a poodle .", "Some dogs do not walk ."), "Some poodles do not walk .", false)
    //    cr.run("U- E+ down", List("Fido is a poodle .", "Some dogs do not walk ."), "A poodle walks .", false)
    //    cr.run("U- E- down", List("Fido is a poodle .", "Some dogs do not walk ."), "No poodles walk .", false)
    //
    //    cr.run("E+ U+ down", List("Fido is a poodle .", "A dog walks ."), "Every poodle walks .", false)
    //    cr.run("E+ U- down", List("Fido is a poodle .", "A dog walks ."), "Some poodles do not walk .", false)
    //    cr.run("E+ E+ down", List("Fido is a poodle .", "A dog walks ."), "A poodle walks .", false)
    //    cr.run("E+ E- down", List("Fido is a poodle .", "A dog walks ."), "No poodles walk .", false)
    //
    //    cr.run("E- U+ down", List("Fido is a poodle .", "No dogs walk ."), "Every poodle walks .", false)
    //    cr.run("E- U- down", List("Fido is a poodle .", "No dogs walk ."), "Some poodles do not walk .", true)
    //    cr.run("E- E+ down", List("Fido is a poodle .", "No dogs walk ."), "A poodle walks .", false)
    //    cr.run("E- E- down", List("Fido is a poodle .", "No dogs walk ."), "No poodles walk .", true)
    //
    //
    //    cr.run("U+ U+ up",   List("Fido is a poodle .", "Every poodle walks ."), "Every dog walks .", false)
    //    cr.run("U+ U- up",   List("Fido is a poodle .", "Every poodle walks ."), "Some dogs do not walk .", false)
    //    cr.run("U+ E+ up",   List("Fido is a poodle .", "Every poodle walks ."), "A dog walks .", true)
    //    cr.run("U+ E- up",   List("Fido is a poodle .", "Every poodle walks ."), "No dogs walk .", false)
    //    
    //    cr.run("U- U+ up",   List("Fido is a poodle .", "Some poodles do not walk ."), "Every dog walks .", false)
    //    cr.run("U- U- up",   List("Fido is a poodle .", "Some poodles do not walk ."), "Some dogs do not walk .", true)
    //    cr.run("U- E+ up",   List("Fido is a poodle .", "Some poodles do not walk ."), "A dog walks .", false)
    //    cr.run("U- E- up",   List("Fido is a poodle .", "Some poodles do not walk ."), "No dogs walk .", false)
    //
    //    cr.run("E+ U+ up",   List("Fido is a poodle .", "A poodle walks ."), "Every dog walks .", false)
    //    cr.run("E+ U- up",   List("Fido is a poodle .", "A poodle walks ."), "Some dogs do not walk .", false)
    //    cr.run("E+ E+ up",   List("Fido is a poodle .", "A poodle walks ."), "A dog walks .", true)
    //    cr.run("E+ E- up",   List("Fido is a poodle .", "A poodle walks ."), "No dogs walk .", false)
    //
    //    cr.run("E- U+ up",   List("Fido is a poodle .", "No poodles walk ."), "Every dog walks .", false)
    //    cr.run("E- U- up",   List("Fido is a poodle .", "No poodles walk ."), "Some dogs do not walk .", true)
    //    cr.run("E- E+ up",   List("Fido is a poodle .", "No poodles walk ."), "A dog walks .", false)
    //    cr.run("E- E- up",   List("Fido is a poodle .", "No poodles walk ."), "No dogs walk .", false)

  }

  class CaseRunner(
    discourseInterpreter: DiscourseInterpreter[DrtExpression] = new BoxerDiscourseInterpreter[DrtExpression](),
    prover: TheoremProver[FolExpression, String] = Prover9TheoremProver.findBinary()) {

    val cache = new scala.collection.mutable.HashMap[List[String], DrtExpression]()

    def run(id: String, assumptions: List[String], goal: String, expected: Option[Boolean]) = {
      val g = cache.getOrElseUpdate(List(goal), discourseInterpreter.interpret(goal).simplify)
      val a = cache.getOrElseUpdate(assumptions, discourseInterpreter.interpretMultisentence(assumptions).simplify)
      val result = prover.prove(List(a.folModal), g.folModal)
      val correct = result.isDefined == (expected.isDefined && expected.get)
      println(id + " result=" + result.isDefined + " expected=" + expected + " correct=" + correct + " " + (if (correct) "" else "*****"))
      println("    " + a)
      println("        " + a.folModal)
      println("    " + g)
      println("        " + g.folModal)
    }
  }

}
