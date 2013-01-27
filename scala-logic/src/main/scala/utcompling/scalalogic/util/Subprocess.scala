package utcompling.scalalogic.util

import scala.sys.process._
import scala.collection.mutable.ListBuffer

class Subprocess {

    def run(process: ProcessBuilder): (Int, String, String) = {
        val out = new ListBuffer[String]
        val err = new ListBuffer[String]
        val exitcode = process ! ProcessLogger(out.append(_), err.append(_))
        return (exitcode, out.mkString("\n"), err.mkString("\n"))
    }

}