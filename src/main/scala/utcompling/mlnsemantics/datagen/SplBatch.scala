package utcompling.mlnsemantics.datagen

import scala.collection.JavaConversions._
import utcompling.scalalogic.util.FileUtils._
import utcompling.scalalogic.util.CollectionUtils._

/**
 * Convert a sentence-per-line file into a file that has 'batchSize'
 * sentences on each line, with a tab in between each sentence.  Each line
 * begins with an integer followed by a tab.
 */
object SplBatch {
  def main(args: Array[String]) {
    val List(inputFile, outputFile, batchSize) = args.toList

    writeUsing(outputFile) { f =>
      for ((batch, batchNum) <- io.Source.fromFile(inputFile).getLines.grouped(batchSize.toInt).zipWithIndex) {
        f.write("%d\t%s\n".format(batchNum * batchSize.toInt + 1, batch.mkString("\t")))
      }
    }
  }
}
