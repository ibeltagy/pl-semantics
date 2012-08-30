package utcompling.mlnsemantics.datagen

import opennlp.scalabha.util.FileUtils

class StsReader(filename: String) extends Iterable[List[String]] {

  override def iterator(): Iterator[List[String]] =
    FileUtils.readLines(filename).map(_.split("\t").toList)

}

object StsReader {
  def main(args: Array[String]): Unit = {
    val r = new StsReader("resources/semantic-textual-similarity/STS.input.MSRvid.txt")
    for (List(a, b) <- r.take(5)) {
      println(a + " | " + b)
    }

  }
}
