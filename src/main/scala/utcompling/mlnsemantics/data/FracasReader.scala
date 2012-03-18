package utcompling.mlnsemantics.data

import scala.xml._
import utcompling.mlnsemantics.datagen.Tokenize

class FracasReader(data: Elem) extends DataReader[RtePair] {

  override def read(): Iterator[RtePair] =
    (for (problem <- (data \ "problem").iterator) yield {
      val Seq(id) = (problem \ "@id").map("fracas-" + _.text)
      val Seq(fracasAnswer) = (problem \ "@fracas_answer").map(_.text)
      val nonstandard = (problem \ "@fracas_nonstandard").map(_.text.toBoolean) match {
        case Seq(b) => b
        case Seq() => false
      }
      val answer = fracasAnswer match {
        case "yes" => Some(true)
        case "no" => Some(false)
        case "unknown" => None
        case "undef" => None
        case _ => throw new RuntimeException(fracasAnswer)
      }
      val premise = (problem \ "p").map(s => cleanSentence(s.text.trim)).toList
      val List(hypothesis) = (problem \ "h").map(s => cleanSentence(s.text.trim)).toList

      if (nonstandard)
        None
      else
        Some(RtePair(id, premise, hypothesis, answer))
    }).flatten

  private def cleanSentence(s: String) = {
    Tokenize(s).mkString(" ")
  }

}

object FracasReader {

  def fromFile(path: String = "resources/fracas/fracas.xml") =
    new FracasReader(XML.loadFile(path))

}