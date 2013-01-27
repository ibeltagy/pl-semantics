package utcompling.scalalogic.top.expression

import utcompling.scalalogic.util.Counter

case class Variable(val name: String) extends Ordered[Variable] {

    def compare(that: Variable): Int =
        this.name.compare(that.name)

}

object Variable {

    private val counter = new Counter

    private val INDVAR_RE = """^[a-df-z]\d*$""".r
    private val FUNCVAR_RE = """^[A-Z]\d*$""".r
    private val EVENTVAR_RE = """^e\d*$""".r

    def isIndVar(expr: String): Boolean =
        INDVAR_RE.findFirstIn(expr).isDefined

    def isFuncVar(expr: String): Boolean =
        FUNCVAR_RE.findFirstIn(expr).isDefined

    def isEventVar(expr: String): Boolean =
        EVENTVAR_RE.findFirstIn(expr).isDefined

    def unique(pattern: Variable): Variable =
        unique(pattern, Set[Variable]())

    def unique(pattern: Variable, exclude: Set[Variable]): Variable = {
        val prefix = pattern.name match {
            case INDVAR_RE() => "z"
            case FUNCVAR_RE() => "F"
            case EVENTVAR_RE() => "e0"
            case _ => "z"
        }
        return unique(prefix, exclude)
    }

    def unique(prefix: String): Variable =
        unique(prefix, Set[Variable]())

    def unique(prefix: String, exclude: Set[Variable]): Variable = {
        var v = new Variable(prefix + counter.get)
        while (exclude.contains(v))
            v = new Variable(prefix + counter.get)
        return v
    }

    class generator(prefix: String, exclude: Set[Variable]) {
        def get() = Variable.unique(prefix, exclude)
    }

}
