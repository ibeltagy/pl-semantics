package utcompling.mlnsemantics.polarity

import utcompling.scalalogic.drt.expression.parse.DrtLogicParser
import utcompling.scalalogic.drt.expression._
import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.top.expression._
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.base.expression._
import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.impl.BoxerDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.call.impl._
import opennlp.scalabha.util.FileUtils
import org.junit.Test
import org.junit.Test

class Tests {

  @Test
  def test() {

    def p(s: String) = new DrtLogicParser().parse(s)
    def f(s: String) = new FolLogicParser().parse(s)

    val discourseInterpreter = new BoxerDiscourseInterpreter[DrtExpression](
      candc = new CandcImpl(),
      boxer = new BoxerImpl())

    val prover = new Prover9TheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/LADR-2009-11A/bin/prover9"), 5, false)
    def tp(a: DrtExpression, g: DrtExpression, s: List[FolExpression] = List()) =
      prover.prove(a.fol +: s, g.fol).isDefined
    def tpf(a: DrtExpression, g: FolExpression, s: List[FolExpression] = List()) =
      prover.prove(a.fol +: s, g).isDefined

    if (false) {
      //            println(discourseInterpreter.interpret("John did not leave .").simplify)

      val a = p("([e1,p2,x0],[john_per(x0), forget(e1), agent(e1,x0), theme(e1,p2), p2:([e3],[leave(e3), agent(e3,x0)])])")
      val gp = p("([e1,x0],[john_per(x0), leave(e1), agent(e1,x0)])")
      val gn = p("([x0],[john_per(x0), -([e1],[leave(e1), agent(e1,x0)])])")

      for (g <- List(gp, gn)) {
        (a -> g).pprint()

        println(tp(a, g))

        println(a.folModal.pretty)
        println(g.folModal.pretty)
      }

    }

    if (false) {
      val a = p("(([x0,x1],[john_per(x0), bill_nam(x1)]) + ([e2,p3],[forget(e2), agent(e2,x0), theme(e2,p3), p3:([e4],[call(e4), agent(e4,x0), patient(e4,x1)])]))")
    }

    if (false) {
      val e = p("([p1,x0],[fido_per(x0), p1:([x2],[dog(x2), (x0 = x2)]), (([x3],[dog(x3)]) -> ([e4],[walk(e4), agent(e4,x3)]))])")
      val a = p("([e1,x0],[dog(x0), walk(e1), agent(e1,x0)])")

      (e -> a).pprint()

      println(tp(e, a))

      println(e.folModal.pretty)
      println(a.folModal.pretty)

      //        val r = f("all x y.R(x,y)")
      //        println(tp(e, a, List(r)))

      val dog = p("([x],[dog(x)])")
      println(dog.folModal.pretty)
      println(tp(e, dog))
      //        println(tp(e, dog, List(r)))
    }

  }
}
