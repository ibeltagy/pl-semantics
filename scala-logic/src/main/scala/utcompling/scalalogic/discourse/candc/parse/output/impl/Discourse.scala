package utcompling.scalalogic.discourse.candc.parse.output.impl
import scala.collection.mutable.ListBuffer

case class Discourse(discourseId: String, sentences: List[Sentence]) {

    override def toString() =
        """Discourse("%s", %s)""".format(discourseId, sentences)

    def repr(): String = {
        //require(sentences.length == 1)

        val sb = new StringBuffer()
        sb.append("{\n")

        val sentStrings = new ListBuffer[String]
        for (s <- sentences) {
            for (w <- s.words)
                sb.append("    val s%d_w%d = %s\n".format(s.index, w.index, w))
            for (w <- s.words)
                sb.append("    s%d_w%d.dependencies = %s\n".format(s.index, w.index, w.dependencies.map { case (k, v) => ('"' + k + '"', v.map(i => "s%d_w%d".format(s.index, i.index))) }))
        }
        sb.append("    Discourse(\"%s\", List(%s))\n".format(discourseId,
            sentences.map(s => "Sentence(%s, List(%s))".format(s.index,
                s.words.map(w => "s%d_w%d".format(s.index, w.index)).mkString(", "))).mkString(", ")))
        sb.append("}")

        return sb.toString
    }

}

case class Sentence(index: Int, words: List[Word]) {

}

case class Word(index: Int, word: String, lemma: String, pos: String, ne: String) {

    private var _dependencies: Map[String, Set[Word]] = null
    val posShort = pos.take(2)

    def dependencies_=(dependencies: Map[String, Set[Word]]) {
        require(this._dependencies == null)
        this._dependencies = dependencies
    }

    def dependencies = _dependencies

    override def toString() =
        """Word(%d, "%s", "%s", "%s", "%s")""".format(index, word, lemma, pos, ne)

}
