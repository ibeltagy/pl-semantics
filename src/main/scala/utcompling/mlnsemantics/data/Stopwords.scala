package utcompling.mlnsemantics.data

object Stopwords {
  lazy val get =
    io.Source.fromFile("src/main/resources/stopwords.txt").getLines
      .filterNot(_.startsWith("#"))
      .map(_.trim)
      .filter(_.nonEmpty)
      .toSet

  def apply(word: String) = get(word)

} 
