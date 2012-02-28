package utcompling.mlnsemantics.datagen

import scala.collection.JavaConversions._
import utcompling.scalalogic.util.FileUtils._
import utcompling.scalalogic.util.CollectionUtils._
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.DiscourseInterpreter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MMap }

object CncLemmatizeCorpus {
  def main(args: Array[String]) {
    val candc = CandcImpl.findBinary(Some(pathjoin(System.getenv("HOME"), "bin/candc/bin")))

    val N = 50000

    for ((sentences, g) <- io.Source.fromFile("data/nytgiga.spl").getLines.map(s => Tokenize(s).mkString(" ")).grouped(N).zipWithIndex.take(2)) {
      val lemmatized = parseToLemmas(candc, sentences)

      //      for ((s, i) <- sentences.zipWithIndex) {
      //        println(s)
      //        println(lemmatized.get(i))
      //        println()
      //      }

      writeUsing("data/nytgiga-cnc-lemma-%03d.spl".format(g)) { f =>
        for (
          i <- 0 to N;
          s <- lemmatized.get(i)
        ) f.write(s.map(_._2).mkString("", " ", "\n"))
      }
    }
  }

  private def parseToLemmas(candc: utcompling.scalalogic.discourse.candc.call.impl.CandcImpl, sentences: Seq[java.lang.String]): scala.collection.immutable.Map[Int, List[(String, String)]] = {
    val candcArgs = Map[String, String](
      "--candc-printer" -> "boxer")
    val candcOut = candc.batchParse(sentences, candcArgs, None, Some("boxer"))
    val outputs = splitOutput(candcOut)
    lemmatize(outputs)
  }

  private def splitOutput(candcOut: String): Map[Int, String] = {
    val outputs = MMap[Int, String]()
    val current = new ListBuffer[String]
    var currNum: Option[Int] = None
    for (line <- candcOut.split("\n")) {
      if (line.startsWith("ccg(")) {
        currNum = Some(line.drop(4).dropRight(1).toInt - 1)
        current.clear()
        current += line
      } else if (currNum.isDefined) {
        if (line.trim.isEmpty) {
          outputs += (currNum.get -> current.mkString("\n"))
          current.clear()
          currNum = None
        } else
          current += line
      }
    }
    outputs.toMap
  }

  private def lemmatize(outputs: Map[Int, String]): scala.collection.immutable.Map[Int, List[(String, String)]] = {
    val TerminalRe = """.*t\(\S+, ?'(\S+)', ?'(\S+)', ?'\S+', ?'\S+', ?'\S+'\).*""".r
    val lemmatized =
      outputs.mapValuesStrict(output =>
        output.split("\n").collect { case TerminalRe(word, lemma) => (word, lemma) }.toList)
    lemmatized
  }
}
