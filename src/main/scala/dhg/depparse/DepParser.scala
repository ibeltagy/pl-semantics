package dhg.depparse

import scala.collection.JavaConverters._
import scala.collection.mutable
import dhg.depparse.ErrorBox._
import dhg.depparse._
import edu.stanford.nlp.ling.CyclicCoreLabel
import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.GrammaticalStructure
import edu.stanford.nlp.trees.PennTreebankLanguagePack
import edu.stanford.nlp.trees.TypedDependency
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.Pattern.{ -> }
import org.apache.commons.logging.LogFactory
//import com.typesafe.scalalogging.log4j.Logging


/**
 * Parse a sentence into a DepGraph dependency graph object.
 *
 * Does a few things worth noting:
 *  - Lemmatizes words (using Stanford lemmatizer)
 */
class DepParser(
  dataLocation: String,
  maxSentenceLength: Int)
  {  //extends Logging {

  private[this] val lp = LexicalizedParser.loadModel(dataLocation, "-maxLength", maxSentenceLength.toString, "-retainTmpSubcategories")
  private[this] val gsf = new PennTreebankLanguagePack().grammaticalStructureFactory

  def apply(sent: String): Option[DepGraph[String, String]] = parse(Tokens.tokenizeToWords(sent))
  def apply(sent: Seq[String]): Option[DepGraph[String, String]] = parse(sent.map(new Word(_)))

  private[this] def parse(sent: Seq[Word]): Option[DepGraph[String, String]] = {
        
    val parsed =
      BooleanAsErrorBox(sent.nonEmpty && sent.length <= maxSentenceLength).errorBox("invalid sentence length")
        .flatMap { _ =>
          val parse = lp.apply(sent.asJava)
          val gs = gsf.newGrammaticalStructure(parse)
          makeDepTree(gs).map(gs -> _)
        }
        .flatMap {
          case (gs, (tree, nodes)) =>
            Enriched_flattenOverErrorBox_GenTraversableLike(gs.typedDependenciesCCprocessed.asScala
              .filter(_.gov.label.index > 0).toVector // remove ROOT edge
              .map { td =>
                val gOpt = OptionAsErrorBox(nodes.get(td.gov.label.index)).errorBox("(for gov) missing node for token " + td.gov.label.index)
                val dOpt = OptionAsErrorBox(nodes.get(td.dep.label.index)).errorBox("(for dep) missing node for token " + td.dep.label.index)
                for (g <- gOpt; d <- dOpt)
                  yield DepRel(Dependency(td.reln.toString), g, d)
              })
              .flattenOverErrorBox
              .map(rels => DepGraph(nodes.map(_._2).toList.sortBy(_.index), rels.toSet, tree, sent.map(_.word).mkString(" ")))
        }
    parsed.forError { error =>
      if (!error.contains("invalid sentence length")) {
        //logger.info("unable to parse: %s: %s".format(error, sent.mkString(" ")))
        println("unable to parse: %s: %s".format(error, sent.mkString(" ")))
      }
    }
    parsed.get
  }

  /**
   * Convert the GrammaticalStructure into a DepNode dependency
   * tree object.
   */
  private[this] def makeDepTree(grammaticalStructure: GrammaticalStructure) = {
    val (indexRelations, nodesByIndexSeq) =
      grammaticalStructure.typedDependencies.asScala.toIndexedSeq.map { (td: TypedDependency) =>
        (td.gov.label.index -> (Dependency(td.reln.toString) -> td.dep.label.index),
          td.dep.label.index -> makeDepNode(td.dep.label))
      }.unzip
    val indexTree = indexRelations.groupByKey // map indices to child indices
    val nodesByIndex = nodesByIndexSeq.toMap //  map from indices to (token, lemma, tag)

    /**
     * Recursively descend indexTree and construct a DepNode tree
     */
    def makeTree(index: Int): ErrorBox[DepNode[String]] = {
      OptionAsErrorBox(nodesByIndex.get(index)).errorBox("(in makeTree) missing node for token " + index)
        .flatMap {
          case (token, lemma, tag, `index`) =>
            val childrenOpt =
              indexTree.getOrElse(index, Vector.empty).mapVals(makeTree)
                .map { case (r, n) => n.map(r -> _) }
                .foldLeft[ErrorBox[Seq[(Dependency, DepNode[String])]]](ErrorBoxValue(Seq())) {
                  case (res, eb) => res.flatMap(s => eb.map(n => s :+ n))
                }
            childrenOpt.map(children => DepNode(token, lemma, tag, index, children))
        }
    }

    OptionAsErrorBox(indexTree.get(0)).errorBox("no root node")
      .flatMap {
        case Seq((r: Root) -> rootIndex) =>
          makeTree(rootIndex).map { tree =>
            val indexedNodes = {
              def inner(t: DepNode[String]): Seq[(Int, DepNode[String])] = (t.index -> t) +: t.children.flatMap(c => inner(c._2))
              inner(tree)
            }.toMap
            (tree, indexedNodes)
          }
      }
  }

  private[this] def makeDepNode(label: CyclicCoreLabel) = {
    (label.word, Lemmatize(label.word, label.tag), label.tag, label.index)
  }
}

object DepParser {
  private val mem: mutable.Map[(String, Int), DepParser] = mutable.Map()

  def load(dataLocation: String, maxSentenceLength: Int): DepParser = mem.getOrElseUpdate((dataLocation, maxSentenceLength), new DepParser(dataLocation, maxSentenceLength))
  def load(maxSentenceLength: Int = 50): DepParser = load("resources/englishPCFG.ser.gz", maxSentenceLength)
}
