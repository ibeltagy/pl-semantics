package utcompling.mlnsemantics.flat

import utcompling.scalalogic.util.Counter
import scala.collection.mutable.HashMap
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.top.expression.Variable

class LabelCounter {

    private val counter = new Counter
    private val propositions = new HashMap[BoxerVariable, String]

    private val PROP_RE = """p\d+""".r

    def get(): String =
        "p" + counter.get

    def get(v: BoxerVariable): String =
        v.name match {
            case PROP_RE() => propositions.getOrElseUpdate(v, this.get)
            case s => s
        }

}