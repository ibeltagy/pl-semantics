package utcompling.mlnsemantics.datagen

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path
import org.apache.hadoop.io.{ IntWritable, Text }
import org.apache.hadoop.mapreduce.{ Job, Mapper, Reducer }
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat
import org.apache.hadoop.util.GenericOptionsParser
import scala.collection.JavaConversions._
import utcompling.scalalogic.util.FileUtils._
import utcompling.scalalogic.util.CollectionUtils._
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.DiscourseInterpreter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MMap }

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
