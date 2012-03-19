package utcompling.mlnsemantics.polarity

import utcompling.scalalogic.util.FileUtils
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.io.Source
import utcompling.scalalogic.discourse.candc.parse.output.impl._

class PolarityLexicon(lexicon: Map[String, List[PolarityLexiconEntry]]) {

  def get(word: Word): (Option[Boolean], Option[Boolean]) =
    lexicon.getOrElse(word.lemma, List()).flatMap(_.get(word)).distinct match {
      case List() => (None, None)
      case List(r) => r
    }

}

object PolarityLexicon {

  private val factRe = """^fact_([pn])\*?$""".r
  private val implRe = """^impl(_p([pn])\*?)?(_n([pn])\*?)?$""".r

  def fromFile(filepath: String): PolarityLexicon = {
    val polarityEntries = new HashMap[String, ListBuffer[PolarityLexiconEntry]]
    Source.fromFile(filepath).getLines.map(println)
    for (line <- Source.fromFile(filepath).getLines.map(_.split("#", 2)(0).trim).filter(_.nonEmpty)) {
      val List(lemma, parcSubcat, pos, requiredRelationsString, relation, signature, example) = line.split("\t").toList

      val (posEnv, negEnv) =
        signature match {
          case factRe(pol) => (isPosEntail(pol), isPosEntail(pol))
          case implRe(_, p, _, n) => (isPosEntail(p), isPosEntail(n))
          case "none" => (None, None)
        }

      val subcat = SubCatFrame(lemma, pos, requiredRelationsString.split(",").map(_.trim).toSet, relation)

      polarityEntries.getOrElseUpdate(lemma, new ListBuffer[PolarityLexiconEntry]) += PolarityLexiconEntry(subcat, posEnv, negEnv)
    }
    return new PolarityLexicon(polarityEntries.mapValues(_.result).toMap)
  }

  private def isPosEntail(s: String): Option[Boolean] =
    Option(s).map(_ == "p")

}

/**
 * @param subCatFrame:
 * @param posEnv: Is the entry positively entailing in positive contexts?  None indicates no entailment.
 * @param negEnv: Is the entry positively entailing in negative contexts?  None indicates no entailment.
 */
case class PolarityLexiconEntry(subCatFrame: SubCatFrame, posEnv: Option[Boolean] = None, negEnv: Option[Boolean] = None) {

  def get(word: Word): Option[(Option[Boolean], Option[Boolean])] =
    if (subCatFrame.matches(word))
      Some(posEnv, negEnv)
    else
      None

}

case class SubCatFrame(lemma: String, pos: String, requiredRelations: Set[String], relation: String) {

  def matches(word: Word): Boolean = {
    if (word.lemma == lemma && word.posShort == pos) {
      val deps = word.dependencies.map(_._1).toList
      for (req <- requiredRelations) {
        if (!deps.exists(_.startsWith(req)))
          return false
      }
      return true
    }
    return false
  }

}

