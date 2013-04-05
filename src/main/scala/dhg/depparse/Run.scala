package dhg.depparse

object Run {

  def main(args: Array[String]) {

    val texts = Vector(
    "John saw the man with a bike.",//,
    "The accident happened as the night was falling"
    //"This is a test.",
    //"Bills on ports and immigration were submitted by Senator Brownback, Republican of Kansas.",
    //"John likes dogs and does not like cats."
    )
    
    val parser = DepParser.load()
    val graphs = texts.map(t => parser.apply(t))
    
    graphs.foreach(_.foreach{g =>
      println(g.source)
      println(g.relations)
      println(g.sourceTree.graphviz)
      println(g.graphviz)
      println(g.logic)
      println
    })
  }

}
