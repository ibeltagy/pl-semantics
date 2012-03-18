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
import utcompling.mlnsemantics.natlog._
import utcompling.scalalogic.util.SeqUtils
import utcompling.scalalogic.util.FileUtils

class ModalDiscourseInterpreter(
  boxerDiscourseInterpreter: DiscourseInterpreter[BoxerExpression] = new BoxerDiscourseInterpreter[BoxerExpression](
    new PassthroughBoxerExpressionInterpreter(),
    CandcImpl.findBinary(Some(FileUtils.pathjoin(System.getenv("HOME"), "bin/candc/bin"))),
    BoxerImpl.findBinary(Some(FileUtils.pathjoin(System.getenv("HOME"), "bin/candc/bin")))),
  candcDiscourseParser: DiscourseParser[Discourse] = new CandcDiscourseParser(CandcImpl.findBinary(Some(FileUtils.pathjoin(System.getenv("HOME"), "bin/candc/bin")))),
  polarityLexicon: PolarityLexicon = PolarityLexicon.fromFile("resources/polarity-lexicon/polarity_lexicon_expanded.txt"))
  extends DiscourseInterpreter[BoxerExpression] {

  override def batchInterpretMultisentence(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[BoxerExpression]] = {
    process(inputs, discourseIds, question, verbose).map(_.map(_._1))
  }

  def process(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[(BoxerExpression, List[BoxerExpression])]] = {
    val newDiscourseIds = discourseIds.getOrElse((0 until inputs.length).map(_.toString).toList)
    val boxerResults = this.boxerDiscourseInterpreter.batchInterpretMultisentence(inputs, Some(newDiscourseIds), question, verbose)
    val parseResults = this.candcDiscourseParser.batchParseMultisentence(inputs, Map(), Some(newDiscourseIds), if (question) Some("question") else Some("boxer"), verbose)
    require(boxerResults.length == parseResults.length)

    for ((boxerResult, parseResult) <- boxerResults zip parseResults) yield {
      if (boxerResult.isEmpty || parseResult.isEmpty)
        None
      else {
        val newRules = this.generateNatlogRules(boxerResult.get, parseResult.get)
        val modalDrs = modalify(boxerResult.get)
        Some((
          if (newRules.nonEmpty)
            BoxerMerge("merge", modalDrs, BoxerDrs(List(), newRules))
          else
            modalDrs,
          newRules))
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def fol(d: BoxerExpression) = new Boxer2DrtExpressionInterpreter().interpret(d).simplify.fol

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private type BoxerRef = (List[BoxerIndex], BoxerVariable)

  private def generateNatlogRules(boxerResult: BoxerExpression, parse: Discourse): List[BoxerExpression] = {
    //println(new Boxer2DrtExpressionInterpreter().interpret(boxerResult))

    val newRules = new ListBuffer[BoxerExpression]
    for (sentence <- parse.sentences; word <- sentence.words) {
      val (posEnv, negEnv) = polarityLexicon.get(word)
      for (BoxerPred(discId, List(BoxerIndex(sentIndex, wordIndex)), _, name, pos, sense) <- findWordInBoxerExpression(word, boxerResult)) {
        if (posEnv.isDefined) {
          val entailmentPred = if (posEnv.get) "POS" else "NEG"
          //newRules += this.parse(discId, sentIndex, wordIndex, "imp(drs([e,p],[pred(%s,%s,e),rel(theme,e,p)]), pred(%s,%s,p))".format(name, pos, entailmentPred, "X"))
          newRules += modalify(this.parse(discId, sentIndex, wordIndex, "imp(drs([e,p],[pred(%s,%s,e),rel(theme,e,p)]), pred(%s,%s,p))".format(name, pos, entailmentPred, "X")))
        }
        if (negEnv.isDefined) {
          val entailmentPred = if (negEnv.get) "POS" else "NEG"
          //newRules += this.parse(discId, sentIndex, wordIndex, "imp(drs([e,p],[not(drs([],[pred(%s,%s,e),rel(theme,e,p)]))]), pred(%s,%s,p))".format(name, pos, entailmentPred, "X"))
          newRules += modalify(this.parse(discId, sentIndex, wordIndex, "imp(drs([p],[not(drs([e],[pred(%s,%s,e),rel(theme,e,p)]))]), pred(%s,%s,p))".format(name, pos, entailmentPred, "X")))
        }
      }
    }

    //        println(fol(modalDrs))
    //        println(fol(modalDrs).pretty)
    //        newRules.map(d => println(fol(d)))
    newRules.result
  }

  private def modalify(e: BoxerExpression): BoxerExpression = {
    //        println(e)
    //        println(new Boxer2DrtExpressionInterpreter().interpret(e).pretty)
    //        println
    val d = e match {
      case BoxerNot(_, _, BoxerNot(_, _, drs)) =>
        modalify(drs)
      case BoxerNot(discId, notIndices, BoxerVerbThemePropDrs(eventRefs, propRefs, eventPreds, eventConds, props, otherRefs, otherConds)) => {
        val negatedEventPreds = eventPreds.map {
          case BoxerPred(discId, verbIndices, variable, name, pos, sense) =>
            BoxerPred(discId, notIndices, variable, notPred(name), pos, sense)
        }
        val propConds = props.flatMap {
          case BoxerProp(discId, propIdx, variable, drs) =>
            List(BoxerImp(discId, propIdx, BoxerPred(discId, propIdx, variable, "POS", "X", 0), modalify(drs)),
              BoxerImp(discId, propIdx, BoxerPred(discId, propIdx, variable, "NEG", "X", 0), modalify(BoxerNot(discId, propIdx, drs))))
        }
        val conds = negatedEventPreds ++ eventConds ++ propConds ++
          (if (otherConds.nonEmpty) List(BoxerNot(discId, notIndices, BoxerDrs(otherRefs, otherConds)))
          else List())
        BoxerDrs(eventRefs ++ propRefs, conds)
      }
      case BoxerVerbThemePropDrs(eventRefs, propRefs, eventPreds, eventConds, props, otherRefs, otherConds) => {
        val propConds = props.flatMap {
          case BoxerProp(discId, propIdx, variable, drs) =>
            List(BoxerImp(discId, propIdx, BoxerPred(discId, propIdx, variable, "POS", "X", 0), modalify(drs)),
              BoxerImp(discId, propIdx, BoxerPred(discId, propIdx, variable, "NEG", "X", 0), modalify(BoxerNot(discId, propIdx, drs))))
        }
        BoxerDrs(eventRefs ++ propRefs, eventPreds ++ eventConds ++ propConds ++ otherConds)
      }
      case BoxerNot(discId, notIndices, BoxerVerbDrs(eventRefs, propRefs, eventPreds, eventConds, otherRefs, otherConds)) => {
        val negatedEventPreds = eventPreds.map {
          case BoxerPred(discId, verbIndices, variable, name, pos, sense) =>
            BoxerPred(discId, notIndices, variable, notPred(name), pos, sense)
        }
        val conds = negatedEventPreds ++ eventConds ++
          (if (otherConds.nonEmpty) List(BoxerNot(discId, notIndices, BoxerDrs(otherRefs, otherConds)))
          else List())
        BoxerDrs(eventRefs ++ propRefs, conds)
      }
      case _ => e.visitConstruct(modalify)
    }

    d match {
      case BoxerDrs(refs1, List(BoxerDrs(refs2, conds))) =>
        modalify(BoxerDrs(refs1 ++ refs2, conds))
      case _ => d
    }
  }

  private def notPred(pred: String) =
    if (pred.startsWith("not_"))
      pred.drop(4)
    else
      "not_" + pred

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

  private def partitionRefs(refs: List[BoxerRef]) = {
    val List(eventRefs, propRefs, otherRefs) = SeqUtils.partitionN(refs, (r: BoxerRef) => r match {
      case (i, BoxerVariable(v)) if """^e\d*$""".r.findFirstIn(v).isDefined => 0
      case (i, BoxerVariable(v)) if """^p\d*$""".r.findFirstIn(v).isDefined => 1
      case _ => 2
    }, Some(3))
    (eventRefs, propRefs, otherRefs)
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

  private def findWordInBoxerExpression(word: Word, ex: BoxerExpression): List[BoxerExpression] =
    return ex match {
      case BoxerNamed(discId, List(BoxerIndex(sentIndex, wordIndex)), variable, name, typ, sense) =>
        if (wordIndex == word.index && name == word.lemma) {
          List(ex)
        }
        else
          List()

      case BoxerPred(discId, List(BoxerIndex(sentIndex, wordIndex)), variable, name, pos, sense) =>
        if (wordIndex == word.index && name == word.lemma) {
          List(ex)
        }
        else
          List()

      case e =>
        e.visit(findWordInBoxerExpression(word, _), (x: List[List[BoxerExpression]]) => x.flatten, List())
    }

}