package utcompling.scalalogic.discourse.candc.boxer.expression

case class BoxerIndex(sentIndex: Int, wordIndex: Int) {

  override def toString() =
    "%d".format(sentIndex * 1000 + wordIndex + 1)

}

object BoxerIndex {

  def apply(index: Int): BoxerIndex =
    BoxerIndex(index / 1000, index % 1000 - 1)

}