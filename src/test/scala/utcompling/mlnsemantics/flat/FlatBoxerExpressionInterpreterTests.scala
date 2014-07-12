package utcompling.mlnsemantics.flat

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
import opennlp.scalabha.util.FileUtils
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import org.junit.Test

class FlatBoxerExpressionInterpreterTests {

  @Test
  def test() {

    val johnForgotToLeave = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1002]:e1,[]:p2],[[1002]:pred(e1,forget,v,0),[1002]:rel(e1,x0,agent,0),[1002]:rel(e1,p2,theme,0),[1002]:prop(p2,drs([[1004]:e3],[[1004]:pred(e3,leave,v,0),[1004]:rel(e3,x0,agent,0)]))]))"
    val johnDidNotForgetToLeave = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1004]:e1,[]:p2],[[1004]:pred(e1,forget,v,0),[1004]:rel(e1,x0,agent,0),[1004]:rel(e1,p2,theme,0),[1004]:prop(p2,drs([[1006]:e3],[[1006]:pred(e3,leave,v,0),[1006]:rel(e3,x0,agent,0)]))]))]))"
    val johnLeft = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1002]:e1],[[1002]:pred(e1,leave,v,0),[1002]:rel(e1,x0,agent,0)]))"
    val johnDidntLeave = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1004]:e1],[[1004]:pred(e1,leave,v,0),[1004]:rel(e1,x0,agent,0)]))]))"
    val someoneLeft = "drs([[1001]:x0,[1002]:e1],[[1002]:pred(e1,leave,v,0),[1002]:rel(e1,x0,agent,0)])"
    val someoneDidntLeft = "drs([[1001]:x0],[[1003]:not(drs([[1004]:e1],[[1004]:pred(e1,leave,v,0),[1004]:rel(e1,x0,agent,0)]))])"

    val everyDogWalks = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,fido,per,0)]),smerge(drs([[1002]:p1],[[1002]:prop(p1,drs([[1003]:x2],[[1004]:pred(x2,dog,n,0),[1002]:eq(x0,x2)]))]),drs([],[[2001]:imp(drs([[2001]:x3],[[2002]:pred(x3,dog,n,0)]),drs([[2003]:e4],[[2003]:pred(e4,walk,v,0),[2003]:rel(e4,x3,agent,0)]))])))"
    val fidoIsADogAndFidoWalks = "alfa(top,drs([[1001]:x0],[[1001,1006]:named(x0,fido,per,0)]),drs([[1002]:p1,[1007]:e2],[[1002]:prop(p1,drs([[1003]:x3],[[1004]:pred(x3,dog,n,0),[1002]:eq(x0,x3)])),[1007]:pred(e2,walk,v,0),[1007]:rel(e2,x0,agent,0)]))"
    val fidoIsADogAndFidoDidNotWalk = "alfa(top,drs([[1001]:x0],[[1001,1006]:named(x0,fido,per,0)]),drs([[1002]:p2],[[1002]:prop(p2,drs([],[[1004]:pred(x0,dog,n,0)])),[1008]:not(drs([[1009]:e3],[[1009]:pred(e3,walk,v,0),[1009]:rel(e3,x0,agent,0)]))]))"
    val aDogWalks = "drs([[1001]:x0,[1003]:e1],[[1002]:pred(x0,dog,n,0),[1003]:pred(e1,walk,v,0),[1003]:rel(e1,x0,agent,0)])"
    val aDogDidNotWalk = "drs([[1001]:x0],[[1002]:pred(x0,dog,n,0),[1004]:not(drs([[1005]:e1],[[1005]:pred(e1,walk,v,0),[1005]:rel(e1,x0,agent,0)]))])"
    val ifRoverIsADogThenRoverWalks = "alfa(top,drs([[1002]:x0,[1007]:x1],[[1002]:named(x0,rover,per,0),[1007]:named(x1,rover,per,0)]),drs([],[[1001]:imp(drs([[1003]:p2],[[1003]:prop(p2,drs([[1004]:x3],[[1005]:pred(x3,dog,n,0),[1003]:eq(x0,x3)]))]),drs([[1008]:e4],[[1008]:pred(e4,walk,v,0),[1008]:rel(e4,x1,agent,0),[1006]:pred(e4,then,a,0)]))]))"

    if (false) {
      val boxerDiscourseInterpreter = new BoxerDiscourseInterpreter[BoxerExpression](
        new PassthroughBoxerExpressionInterpreter(),
        new CandcImpl(),
        new BoxerImpl())
      println(boxerDiscourseInterpreter.interpretMultisentence(List("Fido is a Dog and Fido did not walk .")))
      println(boxerDiscourseInterpreter.interpretMultisentence(List("A dog did not walk .")))
      return
    }

    val p = new BoxerExpressionParser().parse(_)
    def ip(s: String) =
      new FlatPremiseBoxerExpressionInterpreter().interpret(
        List(new MergingBoxerExpressionInterpreterDecorator())
          .map(_.interpret _).reduceLeft(_ andThen _)(p(s)))
    def ih(s: String) =
      new FlatHypothesisBoxerExpressionInterpreter().interpret(
        List(new MergingBoxerExpressionInterpreterDecorator())
          .map(_.interpret _).reduceLeft(_ andThen _)(p(s)))
    val f = new FolLogicParser().parse(_)
    def l(st: String*) = st.map(f).toList
    def d(st: String) = new Boxer2DrtExpressionInterpreter().interpret(p(st))
    def ss(st: String) = println(sideBySide(d(st).simplify.pretty, "   ", "\n" + ip(st).mkString("\n")) + "\n")
    def tpo = new Prover9TheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/LADR-2009-11A/bin/prover9"), 5, false)
    def ftpo = new FlatTheoremProver(tpo, List(new FlatPredicateQuotingInterpreter, new FlatPredicateTypingInterpreter))
    def tp(a: String, g: String, v: Boolean = false) = {
      val (proof, visual) = ftpo.proveVisualize(ip(a), ih(g), v)
      println(sideBySideCentering(d(a).simplify.pretty, "   ", visual, "   ", d(g).simplify.pretty))
    }
    def tpf(a: String, g: String, v: Boolean = false) = {
      val (proof, visual) = ftpo.proveVisualize(ip(a), f(g), v)
      println(sideBySideCentering(d(a).simplify.pretty, "   ", visual))
    }
    def tpp(a: List[String], g: String, v: Boolean = false) = {
      val (proof, visual) = ftpo.proveVisualize(a.map(f), f(g), v)
      println(sideBySideCentering(visual))
    }
    def tpr(a: List[String], g: String, v: Boolean = false) = {
      val proof = tpo.prove(a.map(f), f(g), v)
      println(sideBySideCentering(box(a.map(f(_).pretty).mkString("\n")), " = " + proof.isDefined + " => ", box(f(g).pretty)))
    }
    def spill(a: String, g: String, preds: List[String], v: Boolean = false) = {
      ftpo.spill(ip(a), ih(g), preds, v)
    }

    //spill(fidoIsADogAndFidoDidNotWalk, aDogDidNotWalk, List("truth", "outscopes", "pred", "named", "rel"))
    //spill(fidoIsADogAndFidoDidNotWalk, aDogDidNotWalk, List("pred_x", "eq"))

    //println(ftpo.prove(ip(fidoIsADogAndFidoDidNotWalk), f("""truth("p3", "true")""")).isDefined)
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

    //tp(fidoIsADogAndFidoWalks, aDogWalks)
    //        spill(fidoIsADogAndFidoWalks, aDogWalks, List("true", "outscopes", "pred", "named", "rel"))
    tp(fidoIsADogAndFidoDidNotWalk, aDogDidNotWalk)
    //        spill(fidoIsADogAndFidoDidNotWalk, aDogDidNotWalk, List("true", "outscopes", "pred", "named", "rel"))
    tp(everyDogWalks, aDogWalks)
    //spill(everyDogWalks, aDogWalks, List("true", "pred", "named", "rel"))
    return
    tp(johnForgotToLeave, someoneDidntLeft)
    tp(johnDidNotForgetToLeave, someoneLeft)
    tp(everyDogWalks, ifRoverIsADogThenRoverWalks)

  }
}
