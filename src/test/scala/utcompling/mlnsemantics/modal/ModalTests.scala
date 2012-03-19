package utcompling.mlnsemantics.modal

import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.call.impl._
import utcompling.scalalogic.discourse.impl.BoxerDiscourseInterpreter
import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.util.StringUtils._
import utcompling.scalalogic.util.FileUtils
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.parse.output.impl.Discourse
import utcompling.mlnsemantics.modal.ModalTestsData._
import utcompling.scalalogic.drt.expression.parse.DrtLogicParser
import org.junit.Test

class ModalTests {

  @Test
  def test() {

    def i(drs: String, parse: Discourse): BoxerExpression =
      new ModalDiscourseInterpreter(
        new FakeBoxerDiscourseInterpreter(drs),
        new FakeCandc(parse)).interpret("")

    def p(s: String) = new BoxerExpressionParser().parse(s)
    def f(s: String) = new FolLogicParser().parse(s)
    def drtp(s: String) = new DrtLogicParser().parse(s)
    def l(st: String*) = st.map(f).toList
    def ds(drs: String) = d(p(drs))
    def d(drs: BoxerExpression) = new Boxer2DrtExpressionInterpreter().interpret(drs).simplify
    //        def ss(drs: String, parse: Discourse) = {
    //            println(sideBySide(ds(drs).simplify.pretty, "   ", i(drs, parse).pretty))
    //        }
    def tpo = new Prover9TheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/LADR-2009-11A/bin/prover9"), 5, false)
    def mtpo = new ModalTheoremProver(tpo)
    def vtpo = new VisualizingModalTheoremProverDecorator(mtpo)
    def vwtpo = new VisualizingModalTheoremProverDecorator(new WordnetModalTheoremProverDecorator(mtpo))
    def tp(aDrs: String, aParse: Discourse, gDrs: String, gParse: Discourse, v: Boolean = false) =
      println(vtpo.proveVisualize(List(i(aDrs, aParse)), i(gDrs, gParse), v)._2)
    def tpg(aDrs: String, aParse: Discourse, g: String, v: Boolean = false) =
      println(vtpo.proveVisualize(List(i(aDrs, aParse)), p(g), v)._2)
    def tpr(a: List[String], g: String, v: Boolean = false) = {
      val proof = tpo.prove(a.map(f), f(g), v)
      println(sideBySideCentering(box(a.map(f(_).pretty).mkString("\n")), " = " + proof.isDefined + " => ", box(f(g).pretty)))
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    //        List(johnForgotToLeave_drs -> johnForgotToLeave_parse,
    //            everyDogWalks_drs -> everyDogWalks_parse,
    //            aDogWalks_drs -> aDogWalks_parse)
    //            .map { case (d, p) => ss(d, p) }

    //println(mtpo.prove(List(i(fidoIsADogAndFidoDidNotWalk), f("""truth("p3", "true")""")).isDefined)
    //tpf(fidoIsADogAndFidoDidNotWalk, """pred_("p1", "true", "dog", "x3")""")

    //        tpr(List(
    //            """all p c.(outscopes(p, c) -> (((truth(p, "true") & truth(c, "true")) | (truth(p, "false") & truth(c, "false"))) -> all t n x.(pred_x_(c, t, n, x) -> pred_x_(p, t, n, x))))""",
    //            """pred_x_("p2", "true", "dog", "x2")""",
    //            """outscopes("p1", "p2")""",
    //                
    //            
    //            """all p t1 t2.((truth(p, t1) & truth(p, t2)) -> (t1==t2))""",
    //            """all p.((truth(p,"true") | truth(p,"false")) & (-truth(p,"true") | -truth(p,"false")))""",
    //
    //            """truth("p1", "true")""",
    //            """truth("p2", "true")"""),
    //            
    //            //"""((truth("p1", "true") & truth("p2", "true")) | (truth("p1", "false") & truth("p2", "false")))""",
    //            //"""all t1 t2.((truth("p1", t1) & truth("p2", t2)) -> (t1 = t2))""",
    //            """pred_x_("p1", "true", "dog", "x2")""", 
    //            
    //            false)

    //tp(fidoIsADogAndFidoWalks_drs, fidoIsADogAndFidoWalks_parse, aDogWalks_drs, aDogWalks_parse)
    //tp(fidoIsADogAndFidoDidNotWalk_drs, fidoIsADogAndFidoDidNotWalk_parse, aDogDidNotWalk_drs, aDogDidNotWalk_parse)
    //tp(everyDogWalks_drs, everyDogWalks_parse, aDogWalks_drs, aDogWalks_parse)
    //tp(johnForgotToLeave_drs, johnForgotToLeave_parse, johnDidntLeave_drs, johnDidntLeave_parse)
    //tp(johnDidNotForgetToLeave_drs, johnDidNotForgetToLeave_parse, johnLeft_drs, johnLeft_parse)
    //tp(johnForgotToLeave_drs, johnForgotToLeave_parse, someoneDidntLeft_drs, someoneDidntLeft_parse)
    //tp(everyDogWalks_drs, everyDogWalks_parse, ifRoverIsADogThenRoverWalks_drs, ifRoverIsADogThenRoverWalks_parse)
    //tp(johnHesitatedToLeave_drs, johnHesitatedToLeave_parse, johnLeft_drs, johnLeft_parse)
    //tp(johnHesitatedToLeave_drs, johnHesitatedToLeave_parse, johnDidntLeave_drs, johnDidntLeave_parse)
    //tp(johnDidNotHesitateToLeave_drs, johnDidNotHesitateToLeave_parse, johnLeft_drs, johnLeft_parse)
    //tp(johnDidNotHesitateToLeave_drs, johnDidNotHesitateToLeave_parse, johnDidntLeave_drs, johnDidntLeave_parse)
    //tp(johnDidNotManageToNotForgetToLeave_drs, johnDidNotManageToNotForgetToLeave_parse, johnDidntLeave_drs, johnDidntLeave_parse)
    //tp(johnDidNotManageToNotForgetToLeave_drs, johnDidNotManageToNotForgetToLeave_parse, johnLeft_drs, johnLeft_parse)

    //        tp(johnBoughtAnAutomobile_drs, johnBoughtAnAutomobile_parse, johnBoughtACar_drs, johnBoughtACar_parse)
    //        tp(johnBoughtAnAutomobile_drs, johnBoughtAnAutomobile_parse, johnDidNotBuyACar_drs, johnDidNotBuyACar_parse)
    //        tp(johnDidNotBuyAnAutomobile_drs, johnDidNotBuyAnAutomobile_parse, johnBoughtACar_drs, johnBoughtACar_parse)
    //        tp(johnDidNotBuyAnAutomobile_drs, johnDidNotBuyAnAutomobile_parse, johnDidNotBuyACar_drs, johnDidNotBuyACar_parse)
    //        
    //        tp(johnBoughtACar_drs, johnBoughtACar_parse, johnBoughtAnAutomobile_drs, johnBoughtAnAutomobile_parse)
    //        tp(johnBoughtACar_drs, johnBoughtACar_parse, johnDidNotBuyAnAutomobile_drs, johnDidNotBuyAnAutomobile_parse)
    //        tp(johnDidNotBuyACar_drs, johnDidNotBuyACar_parse, johnBoughtAnAutomobile_drs, johnBoughtAnAutomobile_parse)
    //        tp(johnDidNotBuyACar_drs, johnDidNotBuyACar_parse, johnDidNotBuyAnAutomobile_drs, johnDidNotBuyAnAutomobile_parse)

    //        tp(johnBoughtAConvertible_drs, johnBoughtAConvertible_parse, johnBoughtACar_drs, johnBoughtACar_parse)
    //        tp(johnBoughtAConvertible_drs, johnBoughtAConvertible_parse, johnDidNotBuyACar_drs, johnDidNotBuyACar_parse)
    //        tp(johnDidNotBuyAConvertible_drs, johnDidNotBuyAConvertible_parse, johnBoughtACar_drs, johnBoughtACar_parse)
    //tp(johnDidNotBuyAConvertible_drs, johnDidNotBuyAConvertible_parse, johnDidNotBuyACar_drs, johnDidNotBuyACar_parse) //        
    //        tp(johnBoughtACar_drs, johnBoughtACar_parse, johnBoughtAConvertible_drs, johnBoughtAConvertible_parse)
    //        tp(johnBoughtACar_drs, johnBoughtACar_parse, johnDidNotBuyAConvertible_drs, johnDidNotBuyAConvertible_parse)
    //        tp(johnDidNotBuyACar_drs, johnDidNotBuyACar_parse, johnBoughtAConvertible_drs, johnBoughtAConvertible_parse)
    //tp(johnDidNotBuyACar_drs, johnDidNotBuyACar_parse, johnDidNotBuyAConvertible_drs, johnDidNotBuyAConvertible_parse)

    d(i(johnDidNotManageToLeave_drs, johnDidNotManageToLeave_parse)).pprint()

//    tp(edForgotToLockTheDoor_drs, edForgotToLockTheDoor_parse, edDidNotLockTheDoor_drs, edDidNotLockTheDoor_parse)
//    tp(edForgotThatHeLockedTheDoor_drs, edForgotThatHeLockedTheDoor_parse, edLockedTheDoor_drs, edLockedTheDoor_parse)
//    tp(edForgotHeLockedTheDoor_drs, edForgotHeLockedTheDoor_parse, edLockedTheDoor_drs, edLockedTheDoor_parse)

    //        {
    //            val List(a, c) = List("convertible", "car").map(s => drtp("([x0],[john_per(x0), -([e2,x1],[%s(x1), buy(e2), agent(e2, x0), patient(e2, x1)])])".format(s)))
    //            val b = drtp("([],[([x],[convertible(x)]) -> ([],[car(x)])])")
    //            println(sideBySideCentering(a.pretty, "  ", mtpo.proveVisualizeFol(List(a.fol, b.fol), c.fol)._2, "  ", c.pretty))
    //            println(sideBySideCentering(c.pretty, "  ", mtpo.proveVisualizeFol(List(c.fol, b.fol), a.fol)._2, "  ", a.pretty))
    //        }
    //
    //        {
    //            val List(a, c) = List("convertible", "car").map(s => drtp("([x0],[john_per(x0), ([e2],[not_buy(e2), -([x1],[%s(x1), agent(e2, x0), patient(e2, x1)])])])".format(s)))
    //            val b = drtp("([],[([x],[convertible(x)]) -> ([],[car(x)])])")
    //            println(sideBySideCentering(a.pretty, "  ", mtpo.proveVisualizeFol(List(a.fol, b.fol), c.fol)._2, "  ", c.pretty))
    //            println(sideBySideCentering(c.pretty, "  ", mtpo.proveVisualizeFol(List(c.fol, b.fol), a.fol)._2, "  ", a.pretty))
    //        }

    //        {
    //            val a = drtp("([x],[-([e,p],[verb(e),theme(e,p),P(x)])])")
    //            val c = drtp("([x],[-([e,p],[verb(e),theme(e,p)]),-([][P(x)])])")
    //            println(sideBySideCentering(a.pretty, "  ", mtpo.proveVisualizeFol(List(a.fol), c.fol)._2, "  ", c.pretty))
    //        }

    //
    //
    //
    //

    //        val b = "all p.(-exists e.(forget(e) & theme(e, p)) -> POS(p))"
    //
    //        {
    //            val a = "exists x0.(exists p2.(-exists e1.(forget(e1) & theme(e1, p2) & agent(e1, x0)) & (POS(p2) -> exists e3.(leave(e3) & agent(e3, x0)))))"
    //            val c = "exists e1.(leave(e1))"
    //            println(mtpo.proveVisualizeFol(List(f(a), f(b)), f(c))._2)
    //        }
    //        {
    //            val a = "exists x0.(exists p2.(-exists e1.(forget(e1) & theme(e1, p2)) & (POS(p2) -> exists e3.(leave(e3) & agent(e3, x0)))))"
    //            val c = "exists e1.(leave(e1))"
    //            println(mtpo.proveVisualizeFol(List(f(a), f(b)), f(c))._2)
    //        }
  }

}
