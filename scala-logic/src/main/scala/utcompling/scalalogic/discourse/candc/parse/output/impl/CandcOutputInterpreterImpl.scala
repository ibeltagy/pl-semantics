package utcompling.scalalogic.discourse.candc.parse.output.impl

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MapBuilder
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import utcompling.scalalogic.discourse.candc.parse.output.CandcOutputInterpreter

class CandcOutputInterpreterImpl extends CandcOutputInterpreter[Discourse] {

    private val discourseIdRe = """id\((.*), \[(.*)\]\)\.""".r
    private val singleQuotedRe = """^'(.*)'$""".r
    private val dependencyRe = """\((\S*) (.*)\)""".r
    private val dependencyArgRe = """\S+_(\d+)""".r
    private val wordIndexRe = """^(\S+)_(\d+)$""".r
    private val wordRe = """(\S*)\|(\S*)\|(\S*)\|(\S*)\|(\S*)\|(\S*)""".r

    override def interpret(stdout: String): Map[String, Option[Discourse]] = {
        val discourseDict = new MapBuilder[String, Option[Discourse], Map[String, Option[Discourse]]](Map[String, Option[Discourse]]())
        val rawSentences = new ListBuffer[List[Word]]
        val rawDependencies = new ListBuffer[(String, Int, Int)] //(rel, headIndex, depIndex)
        for (line <- stdout.split("\n").map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#"))) {
            line match {
                case dependencyRe(rel, args) => {
                    val indices = args.split(" ").map {
                        case "_" => None
                        case x => Some(x)
                    }
                    rawDependencies += analyze(rel, indices)
                }

                case _ if line.startsWith("<c>") => {
                    val words = wordRe.findAllIn(line).matchData.map(_.subgroups).zipWithIndex.map {
                        case (List(word, lemma, pos, _, ne, _), index) => Word(index, word, lemma, pos, ne)
                    }.toList
                    rawSentences += words
                    val dependenciesByWord = rawDependencies.groupBy(_._2).map {
                        case (k, v) => (words(k), v.groupBy(_._1).map {
                            case (k, v) => (k, v.map(_._3).toSet.map(words))
                        })
                    }
                    for (word <- words)
                        word.dependencies = dependenciesByWord.getOrElse(word, Map())
                    rawDependencies.clear
                }

                case discourseIdRe(discourseId, rawSentenceIds) => {
                    val cleanDiscourseId = singleQuotedRe.findFirstMatchIn(discourseId).map(_.group(1)).getOrElse(discourseId)
                    val sentenceIds: Array[Int] = rawSentenceIds.split(",").map(_.trim.toInt)
                    require(sentenceIds.length == rawSentences.length, "require(sentenceIds.length=%s == rawSentences.length=%s)".format(sentenceIds.length, rawSentences.length))
                    val sentences = for ((i, s) <- (sentenceIds.toList zipSafe rawSentences)) yield Sentence(i, s)
                    discourseDict += cleanDiscourseId -> Some(Discourse(cleanDiscourseId, sentences))
                    rawSentences.clear
                }
                case _ => None  //TODO: I am not sure if this is correct or not. I added it because for some input strings, 
                				//the outoput parse does not match any of the formats above. This is the sentecen that raised the issue
                //"On my own behalf and on behalf of my colleagues in the Committee on Fisheries, I would ask you, Madam President, 
                //to send Parliament' s condolences to the families of the victims and to the local authorities in both Brittany and
                //in Marín, Galicia, from where the majority of the victims came."
            }
        }
        return discourseDict.result
    }

    /**
     * Take as input a tuple of strings representing
     * a dependency relation in Briscoe/Carroll notation, and
     * convert it into a tuple
     * (relation, relation_subtype, head, dependent)
     *
     * relation is a string
     * relation_subtype is a tuple of strings, or None
     * head, dependent are pairs (string, integer) of a word and
     * an index that describes the position in the sentence.
     */
    private def analyze(rel: String, indices: Array[Option[String]]): (String, Int, Int) = {
        val (subrel, head, dependent) = indices match {
            case Array(head, dependent) if List("dobj", "obj2", "det", "aux", "conj", "iobj").contains(rel) => (None, head, dependent)
            case Array(subrel, head, dependent) if List("xmod", "ncmod", "cmod", "xcomp", "ccomp").contains(rel) => (subrel, head, dependent)
            case Array(head, dependent, subrel) if List("xsubj", "ncsubj", "csubj").contains(rel) => (subrel, head, dependent)
        }
        return (generalize(rel, subrel.map(subtype_analyze)), splitWordIndex(head.get), splitWordIndex(dependent.get))
    }

    private def generalize(rel: String, subrel: Option[String]): String = {
        val subrel_swap = Map("ncsubj" -> "obj")

        var relNew =
            rel match {
                case "xmod" => "mod"
                case "ncmod" => "mod"
                case "cmod" => "mod"
                case "ncsubj" => "subj"
                case "xsubj" => "subj"
                case "csubj" => "subj"
                case "dobj" => "obj"
                case "iobj" => "obj"
                case "obj2" => "obj"
                case "xcomp" => "xcomp"
                case "ccomp" => "ccomp"
                case "det" => "det"
                case "aux" => "aux"
                case "conj" => "conj"
            }

        if (List("ncmod", "xcomp", "ccomp").contains(rel)) { // keep subrelation
            if (subrel.isDefined)
                relNew = relNew + "+" + subrel.get
        } else if (List("cmod", "xmod", "xsubj").contains(rel)) { // drop subrelation

        } else if (List("aux", "conj", "det", "dobj", "iobj", "obj2", "csubj").contains(rel)) { // don't expect any subrel info
            if (subrel.isDefined)
                throw new RuntimeException
        } else if (subrel_swap.contains(rel)) { // replace rel by the subrel, and drop all other subrel info for rel
            if (subrel == subrel_swap.get(rel))
                relNew = subrel.get
        } else {
            throw new RuntimeException
        }

        return relNew
    }

    private def splitWordIndex(s: String) =
        s match {
            case wordIndexRe(word, index) => index.toInt //Pair(word.toLowerCase, index.toInt)
        }

    private def subtype_analyze(subtype: String) =
        subtype match {
            case wordIndexRe(word, index) => word.toLowerCase
            case _ => subtype.toLowerCase
        }

}
