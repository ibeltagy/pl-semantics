package utcompling.scalalogic.util

object StringUtils {

    def sideBySide(strings: String*): String = {
        val structs = strings.map(_.split("\n").toList)
        val maxLines = structs.map(_.length).max
        return structs
            .map(s => s ++ List.fill(maxLines - s.length)(""))
            .map(a =>
                {
                    val maxLineLen = a.map(_.length).max
                    a.map(s => s + (" " * (maxLineLen - s.length)))
                })
            .transpose
            .map(_.mkString("")).mkString("\n")
    }

    def sideBySideCentering(strings: String*): String = {
        val structs = strings.map(_.split("\n").toList)
        val maxLines = structs.map(_.length).max
        val structs2 = structs.map(s => List.fill((maxLines - s.length) / 2)("") ++ s)
        return sideBySide(structs2.map(_.mkString("\n")): _*)
    }

    def box(a: Any): String = {
        val s = a.toString
        val sHeight = s.split("\n").length
        val sWidth = s.split("\n").map(_.length).max
        val wall = List.fill(sHeight)("|").mkString("\n")
        val top = " _" + ("_" * sWidth) + "_ "
        val bottom = "|_" + ("_" * sWidth) + "_|"
        return top + "\n" + StringUtils.sideBySide(wall, " ", s, " ", wall) + "\n" + bottom
    }

}
