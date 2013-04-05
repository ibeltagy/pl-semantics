package dhg.depparse

/**
 * Dependency Tree
 */
case class DepNode[L: Ordering](token: String, lemma: L, tag: String, index: Int, children: Seq[(Dependency, DepNode[String])]) {
  def flatten: Seq[DepNode[String]] = {
    val lem = lemma match { case s: String => s; case x: HasWord => x.name }
    DepNode(token, lem, tag, index, children) +: children.flatMap(_._2.flatten)
  }

  override def toString: String = "%s-%s-%s".format(token, tag, index)

  def graphviz =
    ("digraph G {" +:
      (for (
        DepNode(token, lem, tag, index, children) <- flatten;
        (reln, DepNode(cToken, cLem, cTag, cIndex, _)) <- children.sortBy(_._2.index)
      ) yield {
        """  "%s-%s-%s" -> "%s-%s-%s" [ label = "%s" ]""".format(token, tag, index, cToken, cTag, cIndex, reln.value)
      }) :+ "}").mkString("\n")
}

object DepNode {
  def apply[L: Ordering](lemma: L) = new DepNode("", lemma, "", -1, null)
}
