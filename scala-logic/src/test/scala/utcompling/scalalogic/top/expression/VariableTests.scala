package utcompling.scalalogic.top.expression

import scala.collection.mutable.MapBuilder
import org.junit.Test

class VariableTests {
  
  @Test
  def test() {
      
      println(Variable("a") == Variable("a"))
      println(Variable("a").equals(Variable("a")))
      println(Variable("b") == Variable("a"))
      println(Variable("b").equals(Variable("a")))

      val hmb = new MapBuilder[Variable,Int,Map[Variable,Int]](Map[Variable,Int]())
      hmb += Variable("a") -> 1
      hmb += Variable("b") -> 2
      hmb += Variable("a") -> 3
      println(hmb.result)
      
      println(List(Variable("b"),Variable("a")).sorted)
      
      println(Variable.unique(Variable("z100")))
      println(Variable.unique(Variable("z100")))
      println(Variable.unique(Variable("z100"), Set(Variable("z2"),Variable("z3"),Variable("z4"))))

  }
}
