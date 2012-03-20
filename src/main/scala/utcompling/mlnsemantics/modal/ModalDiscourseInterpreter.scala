package utcompling.mlnsemantics.modal

import utcompling.scalalogic.discourse._
import utcompling.scalalogic.discourse.impl._
import utcompling.scalalogic.discourse.candc.boxer._
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.scalalogic.drt.expression._
import utcompling.scalalogic.top.expression._
import utcompling.scalalogic.discourse.candc.call._
import utcompling.scalalogic.discourse.candc.call.impl._
import utcompling.scalalogic.discourse.candc.parse.output.impl._
import scala.collection.mutable.ListBuffer
import utcompling.mlnsemantics.polarity._
import utcompling.scalalogic.util.SeqUtils
import utcompling.scalalogic.util.FileUtils
import org.apache.commons.logging.LogFactory

/**
 * Discourse Interpreter that decorates a logical form.
 *
 * Makes use of both Boxer and C&C outputs for logical form and dependency
 * information, respectively.  Additionally, uses the polarity lexicon.
 *
 * This decorator works as follows:
 *
 *
 *
 *
 */
class ModalDiscourseInterpreter(
  delegate: DiscourseInterpreter[BoxerExpression] = new BoxerDiscourseInterpreter[BoxerExpression](
    new PassthroughBoxerExpressionInterpreter(),
    CandcImpl.findBinary(Some(FileUtils.pathjoin(System.getenv("HOME"), "bin/candc/bin"))),
    BoxerImpl.findBinary(Some(FileUtils.pathjoin(System.getenv("HOME"), "bin/candc/bin")))),
  candcDiscourseParser: DiscourseParser[Discourse] = new CandcDiscourseParser(CandcImpl.findBinary(Some(FileUtils.pathjoin(System.getenv("HOME"), "bin/candc/bin")))),
  polarityLexicon: PolarityLexicon = PolarityLexicon.fromFile("resources/polarity-lexicon/polarity_lexicon_expanded.txt"))
  extends DiscourseInterpreter[BoxerExpression] {

  protected val LOG = LogFactory.getLog(classOf[ModalDiscourseInterpreter])

  private type BoxerRef = (List[BoxerIndex], BoxerVariable)

  object NotPrefix {
    val NotPrefixRe = """^not_(.+)$""".r
    def unapply(pred: String) =
      pred match {
        case NotPrefixRe(unnegated) => Some(unnegated)
        case _ => None
      }
    def apply(pred: String) = "not_" + pred
  }

  val EventVar = """^e\d*$""".r
  val PropVar = """^p\d*$""".r

  /**
   * Hook to which all interpret calls delegate.  Calls `process`.
   */
  override def batchInterpretMultisentence(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[BoxerExpression]] = {
    process(inputs, discourseIds, question).map(_.map(_._1))
  }

  /**
   * @param inputs			natural language discourses
   * @param discourseIds
   * @param question
   * @return 				list of (augmented) BoxerExpressons and list of new rules added to that expression
   */
  def process(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[(BoxerExpression, List[BoxerExpression])]] = {

    val newDiscourseIds = discourseIds.getOrElse((0 until inputs.length).map(_.toString).toList)
    val boxerResults = delegate.batchInterpretMultisentence(inputs, Some(newDiscourseIds), question, verbose)
    val parseResults = candcDiscourseParser.batchParseMultisentence(inputs, Map(), Some(newDiscourseIds), if (question) Some("question") else Some("boxer"), verbose)
    require(boxerResults.length == parseResults.length)

    for (
      (boxerResultOpt, parseResultOpt) <- (boxerResults zip parseResults);
      boxerResult <- boxerResultOpt;
      parseResult <- parseResultOpt
    ) yield {
      val modalDrs = modalify(boxerResult)
      val newRules = this.generateNatlogRules(boxerResult, parseResult)
      val resultDrs = if (newRules.nonEmpty) BoxerMerge("merge", modalDrs, BoxerDrs(List(), newRules)) else modalDrs
      Some(resultDrs, newRules)
    }

  }

  /**
   * Take a logical form and its corresponding dependency parse; generate inference rules.
   *
   * For each word of each sentence in the parse, check for an entry in the
   * polarity lexicon.  The lexicon is indexed by word and required
   * dependencies. For any words found in the lexicon, locate the word in the
   * logical form (it will be a BoxerPred).
   *
   * For each entry in the polarity lexicon, generate positive and/or negative
   * environment modality rules.  For example, if a word has positive entailment
   * in a negative environment, it will generate a BoxerExpression equivalent to:
   *  ____________________
   * | p                  |
   * |--------------------|
   * |      ____________  |
   * | __  | e          | |
   * |   | |------------| | => POS(X)
   * |     | word(e)    | |
   * |     | theme(e,p) | |
   * |     |____________| |
   * |____________________|
   */
  private def generateNatlogRules(boxerResult: BoxerExpression, parse: Discourse): List[BoxerExpression] = {
    val allWords = parse.sentences.flatMap(_.words)
    allWords.flatMap {
      word =>
        val (posEnv, negEnv) = polarityLexicon.get(word)
        if (posEnv.isDefined || negEnv.isDefined) {
          findWordInBoxerExpression(word, boxerResult).flatMap {
            case BoxerPred(discId, List(BoxerIndex(sentIndex, wordIndex)), _, name, pos, sense) =>
              List(
                posEnv.map { env =>
                  val entailmentPred = if (env) "POS" else "NEG"
                  modalify(this.parse(discId, sentIndex, wordIndex, "imp(drs([e,p],[pred(%s,%s,e),rel(theme,e,p)]), pred(%s,%s,p))".format(name, pos, entailmentPred, "X")))
                },
                negEnv.map { env =>
                  val entailmentPred = if (env) "POS" else "NEG"
                  modalify(this.parse(discId, sentIndex, wordIndex, "imp(drs([p],[not(drs([e],[pred(%s,%s,e),rel(theme,e,p)]))]), pred(%s,%s,p))".format(name, pos, entailmentPred, "X")))
                }).flatten
            //case BoxerPred(discId, List(BoxerIndex(sentIndex, wordIndex)), _, name, pos, sense) => // TODO: ...
          }
        }
        else List()
    }
  }

  /**
   * Transform the BoxerExpression to the "modal" style.
   *
   * This means to do the following things:
   *
   * 1) Eliminate double-negations
   *
   * 2) Identify negated DRSs that have an event.
   *    Push the negation into the DRS by using the modal constructs to
   *    describe the negation.
   *    - Add the prefix "not_" the any relevant verbs (a verb of the event).
   *    - Made a negated DRS containing all other conditions.
   *
   * 3) Identify DRSs that have an event and a theme proposition.
   *    - For relevant propositions P, add implications of the form
   *      POS(X) => P and NEG(X) => -P
   *
   * 4) Identify negated DRSs that have an event and a theme proposition.
   *    Push the negation into the DRS by using the modal constructs to
   *    describe the negation.
   *    - Add the prefix "not_" the any relevant verbs (a verb of the event
   *      and that has the proposition as its theme).
   *    - For relevant propositions P, add implications of the form
   *      POS(X) => P and NEG(X) => -P
   *    - Made a negated DRS containing all other conditions.
   */
  private def modalify(e: BoxerExpression): BoxerExpression = {
    val d = e match {
      case BoxerNot(_, _, BoxerNot(_, _, drs)) =>
        modalify(drs)
      case BoxerNot(discId, notIndices, BoxerVerbDrs(eventRefs, propRefs, eventPreds, eventConds, otherRefs, otherConds)) => {
        val negatedEventPreds = eventPreds.map {
          case BoxerPred(discId, verbIndices, variable, name, pos, sense) =>
            BoxerPred(discId, notIndices, variable, notPred(name), pos, sense)
        }
        val conds = negatedEventPreds ++ eventConds ++
          (if (otherConds.nonEmpty) List(BoxerNot(discId, notIndices, BoxerDrs(otherRefs, otherConds))) else List())
        BoxerDrs(eventRefs ++ propRefs, conds)
      }
      case BoxerVerbThemePropDrs(eventRefs, propRefs, eventPreds, eventConds, props, otherRefs, otherConds) => {
        val propConds = props.flatMap {
          case BoxerProp(discId, propIdx, variable, drs) =>
            List(
              BoxerImp(discId, propIdx, BoxerPred(discId, propIdx, variable, "POS", "X", 0), modalify(drs)),
              BoxerImp(discId, propIdx, BoxerPred(discId, propIdx, variable, "NEG", "X", 0), modalify(BoxerNot(discId, propIdx, drs))))
        }
        BoxerDrs(eventRefs ++ propRefs, eventPreds ++ eventConds ++ propConds ++ otherConds)
      }
      case BoxerNot(discId, notIndices, BoxerVerbThemePropDrs(eventRefs, propRefs, eventPreds, eventConds, props, otherRefs, otherConds)) => {
        val negatedEventPreds = eventPreds.map {
          case BoxerPred(discId, verbIndices, variable, name, pos, sense) =>
            BoxerPred(discId, notIndices, variable, notPred(name), pos, sense)
        }
        val propConds = props.flatMap {
          case BoxerProp(discId, propIdx, variable, drs) =>
            List(
              BoxerImp(discId, propIdx, BoxerPred(discId, propIdx, variable, "POS", "X", 0), modalify(drs)),
              BoxerImp(discId, propIdx, BoxerPred(discId, propIdx, variable, "NEG", "X", 0), modalify(BoxerNot(discId, propIdx, drs))))
        }
        val conds = negatedEventPreds ++ eventConds ++ propConds ++
          (if (otherConds.nonEmpty) List(BoxerNot(discId, notIndices, BoxerDrs(otherRefs, otherConds))) else List())
        BoxerDrs(eventRefs ++ propRefs, conds)
      }
      case _ => e.visitConstruct(modalify)
    }

    d match {
      case BoxerDrs(refs1, List(BoxerDrs(refs2, conds))) => modalify(BoxerDrs(refs1 ++ refs2, conds))
      case _ => d
    }
  }

  /**
   * For an input predicate string, if it begins with the prefix "not_",
   * remove the prefix; if it doesn't have the prefix, add it.
   */
  private def notPred(pred: String) =
    pred match {
      case NotPrefix(unNegated) => unNegated
      case _ => NotPrefix(pred)
    }

  /**
   * Matches DRS objects that have an event and a theme proposition
   *
   * @return (eventRefs, propRefs, eventPreds, eventConds, otherRefs, otherConds)
   */
  private object BoxerVerbThemePropDrs {
    def unapply(d: BoxerDrs): Option[(List[BoxerRef], List[BoxerRef], List[BoxerExpression], List[BoxerExpression], List[BoxerExpression], List[BoxerRef], List[BoxerExpression])] = d match {
      case BoxerDrs(refs, conds) => {
        val (eventRefs, propRefs, otherRefs) = partitionRefs(refs)
        if (eventRefs.nonEmpty && propRefs.nonEmpty) {
          val eventVars = eventRefs.map(_._2)
          val propVars = propRefs.map(_._2)
          val List(eventPreds, eventConds, propConds, otherConds) = SeqUtils.partitionN(conds, (be: BoxerExpression) => be match {
            case BoxerPred(_, _, e, _, _, _) if eventVars.contains(e) => 0
            case BoxerRel(_, _, e, p, _, _) if eventVars.contains(e) && propVars.contains(p) => 1
            case BoxerProp(_, _, p, _) if propVars.contains(p) => 2
            case _ => 3
          }, Some(4))
          return Some((eventRefs, propRefs, eventPreds, eventConds, propConds, otherRefs, otherConds))
        }
        return None
      }
      case _ => {
        return None
      }
    }
  }

  /**
   * Matches DRS objects that have an event
   */
  private object BoxerVerbDrs {
    def unapply(d: BoxerDrs): Option[(List[BoxerRef], List[BoxerRef], List[BoxerExpression], List[BoxerExpression], List[BoxerRef], List[BoxerExpression])] = d match {
      case BoxerDrs(refs, conds) => {
        val (eventRefs, propRefs, otherRefs) = partitionRefs(refs)
        if (eventRefs.nonEmpty) {
          val eventVars = eventRefs.map(_._2)
          val propVars = propRefs.map(_._2)
          val List(eventPreds, eventConds, otherConds) = SeqUtils.partitionN(conds, (be: BoxerExpression) => be match {
            case BoxerPred(_, _, e, _, _, _) if eventVars.contains(e) => 0
            case BoxerRel(_, _, e, p, _, _) if eventVars.contains(e) && propVars.contains(p) => 1
            case _ => 2
          }, Some(3))
          return Some((eventRefs, propRefs, eventPreds, eventConds, otherRefs, otherConds))
        }
        return None
      }
      case _ =>
        return None
    }
  }

  /**
   * Split a list of refs into separate lists for events, propositions, and
   * everything else.
   */
  private def partitionRefs(refs: List[BoxerRef]) = {
    val List(eventRefs, propRefs, otherRefs) =
      SeqUtils.partitionN(refs, (r: BoxerRef) =>
        r match {
          case (i, BoxerVariable(EventVar())) => 0
          case (i, BoxerVariable(PropVar())) => 1
          case _ => 2
        }, Some(3))
    (eventRefs, propRefs, otherRefs)
  }

  /**
   * Given a word from a parse, find the corresponding BoxerPred in the
   * BoxerExpression.
   */
  private def findWordInBoxerExpression(word: Word, ex: BoxerExpression): List[BoxerExpression] =
    return ex match {
      case BoxerNamed(discId, List(BoxerIndex(sentIndex, wordIndex)), variable, name, typ, sense) =>
        if (wordIndex == word.index && name == word.lemma) List(ex) else List()
      case BoxerPred(discId, List(BoxerIndex(sentIndex, wordIndex)), variable, name, pos, sense) =>
        if (wordIndex == word.index && name == word.lemma) List(ex) else List()
      case e =>
        e.visit(findWordInBoxerExpression(word, _), (x: List[List[BoxerExpression]]) => x.flatten, List())
    }

  private def parse(discId: String, sentIndex: Int, wordIndex: Int, expression: String): BoxerExpression = {
    def doRefs(e: List[String]): (List[BoxerVariable], List[String]) =
      e match {
        case "]" :: tail => (Nil, tail)
        case v :: "," :: tail => doRefs(tail) match { case (refs, r) => (BoxerVariable(v) :: refs, r) }
        case v :: "]" :: tail => (BoxerVariable(v) :: Nil, tail)
      }

    def doConds(ex: List[String]): (List[BoxerExpression], List[String]) =
      doParse(ex) match {
        case (e, "," :: tail) => doConds(tail) match { case (conds, r) => (e :: conds, r) }
        case (e, "]" :: tail) => (e :: Nil, tail)
      }

    def doParse(e: List[String]): (BoxerExpression, List[String]) =
      e match {
        case "drs" :: "(" :: "[" :: tail => {
          val (refs, "," :: "[" :: r1) = doRefs(tail)
          val (conds, ")" :: r2) = doConds(r1)
          (BoxerDrs(refs.map(List() -> _), conds), r2)
        }
        case "imp" :: "(" :: tail => {
          val (first, "," :: r1) = doParse(tail)
          val (second, ")" :: r2) = doParse(r1)
          (BoxerImp(discId, List(BoxerIndex(sentIndex, wordIndex)), first, second), r2)
        }
        case "not" :: "(" :: tail => {
          val (d, ")" :: r) = doParse(tail)
          (BoxerNot(discId, List(BoxerIndex(sentIndex, wordIndex)), d), r)
        }
        case "pred" :: "(" :: name :: "," :: pos :: "," :: v :: ")" :: tail =>
          (BoxerPred(discId, List(BoxerIndex(sentIndex, wordIndex)), BoxerVariable(v), name, pos, 0), tail)
        case "rel" :: "(" :: name :: "," :: v1 :: "," :: v2 :: ")" :: tail =>
          (BoxerRel(discId, List(BoxerIndex(sentIndex, wordIndex)), BoxerVariable(v1), BoxerVariable(v2), name, 0), tail)
        case _ =>
          throw new RuntimeException(e.map('"' + _ + '"').toString)
      }

    val (be, Nil) = doParse("""\s+""".r.split("()[],".map(_.toString).foldLeft(expression)((a, b) => a.replace(b, " " + b + " "))).toList)
    return be
  }

}