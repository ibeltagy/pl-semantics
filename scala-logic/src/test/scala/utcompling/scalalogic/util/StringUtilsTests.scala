package utcompling.scalalogic.util

import org.junit.Test

class StringUtilsTests {
  def test() {

    val a = """------------------------
               |                      |
               |                      |
               |                      |
               |                      |
               |                      |
               |                      |
               |                      |
               |                      |
               |                      |
               ------------------------""".split("\n").map(_.trim).mkString("\n")
    val b = """pred_(p1, forget, e1)
               rel_(p1, agent, e1, x0)
               rel_(p1, theme, e1, p2)
               prop_(p1, p2)
               pred_(p2, leave, e3)
               rel_(p2, agent, e3, x0)""".split("\n").map(_.trim).mkString("\n")
    val c = "  ==>  "
    val d = """------------------------
               |                      |
               |                      |
               |                      |
               ------------------------""".split("\n").map(_.trim).mkString("\n")

    println(StringUtils.sideBySide(a, b, c, d))
    println(StringUtils.sideBySideCentering(a, b, c, d))

    println(StringUtils.box(b))

  }
}
