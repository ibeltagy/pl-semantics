package utcompling.mlnsemantics.datagen

import scala.collection.JavaConversions._
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.CollectionUtils._

/**
 * Convert a sentence-per-line file into a file that has 'batchSize'
 * sentences on each line, with a tab in between each sentence.  Each line
 * begins with an integer followed by a tab.
 *
 * sbt start-script; target/start utcompling.mlnsemantics.datagen.SplBatch /scratch/01899/dhg1/nytgiga.spl /scratch/01899/dhg1/nytgiga.grouped 50000000 300
 */
object SplBatch {
  def main(args: Array[String]) {
    val List(inputFile, outputFile, sentencesPerFile, batchSize) = args.toList

//    for((fileSents, fileNum) <- io.Source.fromFile(inputFile).getLines.grouped(sentencesPerFile.toInt).zipWithIndex)
//      writeUsing(outputFile+"-%d".format(fileNum)) { f =>
//        for ((batch, batchNum) <- fileSents.grouped(batchSize.toInt).zipWithIndex)
//          f.write("%d\t%s\n".format(fileNum * sentencesPerFile.toInt + batchNum * batchSize.toInt + 1, batch.mkString("\t")))
//      }

      writeUsing(outputFile) { f =>
        for ((batch, batchNum) <- io.Source.fromFile(inputFile).getLines.grouped(batchSize.toInt).zipWithIndex)
          f.write("%d\t%s\n".format(batchNum * batchSize.toInt + 1, batch.mkString("\t")))
      }

  }
}

