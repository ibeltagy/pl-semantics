package utcompling.mlnsemantics.datagen

import io.Source

class StsReader(filename: String) extends Iterable[List[String]] {

  override def iterator() = new Iterator[List[String]] {
    val lines = Source.fromFile(filename).getLines
    override def next() = lines.next.split("\t").toList
    override def hasNext(): Boolean = lines.hasNext
  }

}

object StsReader {
  def main(args: Array[String]): Unit = {
    val r = new StsReader("resources/semantic-textual-similarity/STS.input.MSRvid.txt")
    for (List(a, b) <- r.take(5)) {
      println(a + " | " + b)
    }

  }
}
