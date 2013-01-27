package utcompling.scalalogic.discourse.candc.boxer.expression.parse

import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import org.junit.Test

class BoxerExpressionParserTests {

  @Test
  def test() {

    val p = new BoxerExpressionParser().parse(_)

    var drs = List("""drs(
                            [],
                            [
                              [1001]:imp(
                                drs(
                                  [[1001]:x0],
                                  [
                                    [1002]:pred(x0,man,n,0)
                                  ]
                                ),
                                drs(
                                  [[1004]:x1,[1003]:e2],
                                  [
                                    [1005]:pred(x1,woman,n,0),
                                    [1003]:pred(e2,love,v,0),
                                    [1003]:rel(e2,x0,agent,0),
                                    [1003]:rel(e2,x1,patient,0)
                                  ]
                                )
                              )
                            ]
                          )""",

      """alfa(
                            top,
                            drs(
                              [[1001]:x0,[1005]:x1],
                              [
                                [1001]:named(x0,john,per,0),
                                [1005]:named(x1,bill,nam,0)
                              ]
                            ),
                            drs(
                              [[1002]:e2,[]:p3],
                              [
                                [1002]:pred(e2,forget,v,0),
                                [1002]:rel(e2,x0,agent,0),
                                [1002]:rel(e2,p3,theme,0),
                                [1002]:prop(
                                  p3,
                                  drs(
                                    [[1004]:e4],
                                    [
                                      [1004]:pred(e4,call,v,0),
                                      [1004]:rel(e4,x0,agent,0),
                                      [1004]:rel(e4,x1,patient,0)
                                    ]
                                  )
                                )
                              ]
                            )
                          )""",

      """alfa(
                               top,
                               drs(
                                 [[1001]:x0],
                                 [
                                   [1001]:named(x0,fido,per,0)
                                 ]
                               ),
                               smerge(
                                 drs(
                                   [[1002]:p1],
                                   [
                                     [1002]:prop(
                                       p1,
                                       drs(
                                         [[1003]:x2],
                                         [
                                           [1004]:pred(x2,dog,n,0),
                                           [1002]:eq(x0,x2)
                                         ]
                                       )
                                     )
                                   ]
                                 ),
                                 drs(
                                   [],
                                   [
                                     [2001]:imp(
                                       drs(
                                         [[2001]:x3],
                                         [
                                           [2002]:pred(x3,dog,n,0)
                                         ]
                                       ),
                                       drs(
                                         [[2003]:e4],
                                         [
                                           [2003]:pred(e4,walk,v,0),
                                           [2003]:rel(e4,x3,agent,0)
                                         ]
                                       )
                                     )
                                   ]
                                 )
                               )
                             )""",
      "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1002]:e1],[[1002]:pred(e1,leave,v,0),[1002]:rel(e1,x0,agent,0)]))",
      "drs([[1001]:x0,[1003]:e1],[[1002]:pred(x0,dog,n,0),[1003]:pred(e1,walk,v,0),[1003]:rel(e1,x0,agent,0)])",
      "drs([[1001]:x0],[[1002]:pred(x0,left,n,0),[]:pred(x0,topic,a,1)])")

    for (d <- drs) {
      var drsFlat = """[ \t\n]+""".r.replaceAllIn(d, "")
      println(drsFlat)
      println(p(d))
      var reparsed = p(d).toString
      var error = false
      if (drsFlat != reparsed) {
        println((drsFlat zip reparsed).map { case (a, b) => (if (a == b || a - 1 == b) ' ' else '^') }.mkString(""))
        error = true
      }
      println(error)
      new Boxer2DrtExpressionInterpreter().interpret(p(d)).pprint()
      new Boxer2DrtExpressionInterpreter().interpret(p(d)).simplify.pprint()
      println()
    }
  }
}
