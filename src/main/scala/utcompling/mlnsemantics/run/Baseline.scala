package utcompling.mlnsemantics.run

import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import opennlp.scalabha.util.FileUtils
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.mlnsemantics.vecspace._
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.UnnecessarySubboxRemovingBoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import org.apache.log4j.Logger
import org.apache.log4j.Level
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.PredicateCleaningBoxerExpressionInterpreterDecorator
import utcompling.mlnsemantics.inference._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.inference.support.WeightedExpression
import scala.collection.mutable.ListBuffer
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.mlnsemantics.rules.RuleType
import scalax.collection.edge.WDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.mutable.Graph
import scalax.collection.edge.LUnDiEdge
import scalax.collection.edge.LkUnDiEdge

object Baseline {

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)

    //    val txt = "an architect bought a new red car"
    //    val hyp = "a person purchased a new vehicle"

    val a = List(
      List("A man is riding a bicycle ."),
      List("A man is riding a bike ."),

      List("A stadium craze is sweeping the country ."),
      List("A stadium craze is covering the country ."), // True

      List("He left the children with the nurse ."),
      List("He entrusted the children to the nurse ."), // False without "all e all x ((leave_v_dt_1002(e) & r_with(e, x)) -> (entrust_v_dh_2002(e) & r_to(e, x)))"

      List("He left the children with the nurse ."),
      List("He entrusted the children with the nurse ."),

      List("South Korea fails to honor U.S. patents ."),
      List("South Korea does not observe U.S. patents ."), // True

      List("The U.S. is watching closely as South Korea fails to honor U.S. patents ."),
      List("South Korea does not observe U.S. patents ."), // False, stuff after "as" is treated as a nested subexpression

      List("We return to the young woman who is reading the Wrigley's wrapping paper ."),
      List("The young woman reading the Wrigley's wrapping paper is revisited ."), // False, "return to" -> "revisit" since the "to" doesn't get dropped

      //      List("After a fire extinguisher is used, it must always be returned for recharging and its use recorded ."),
      //      List("A fire extinguisher must always be sent back after using it ."), // ERROR, Boxer doesn't handle "after" right

      List("Joe Robbie could not persuade the mayor , so he built his own coliseum .", "He has used it to turn a healthy profit ."), // NOTE: "couldn't" -> "could not"
      List("Joe Robbie used a stadium to turn a sizable profit ."), // False, "persuade" doesn't produce a "theme" predicate

      List("The significance of this will be apparent when it is realized that , while the sportsman is trying to get a sight of the tiger , the tiger in all probability is trying to stalk the sportsman ."),
      List("The tiger is following the sportsman ."),

      List("Mary 's grandfather taught her necessary skills : how to tip my tea into my saucer and blow waves across it until it was cool enough to drink ; how to cut an orange in half crossways and pack a sugar lump into each half and then suck out orange-juice and sugar together ; how to walk along the crazy-paving garden path without stepping on any of the cracks or a tiger would get you ; how to butter the loaf and then clutch it to your chest and then shave off paper-thin slices ; what saint to pray to when you woke up at night and saw the devil moving behind the curtains ."),
      List("Mary was instructed some useful skills by her grandfather ."))

    //
    //
    //

    val vsf1 = (words: (String => Boolean)) => BowVectorSpace.nullVectorSpace
    val vsf2 = (words: (String => Boolean)) => BowVectorSpace("resources/nytgiga.lem.1m.vc.f2000.m50.wInf", words)

    val ttp =
      new TextualTheoremProver(
        new ModalDiscourseInterpreter(),
        new InferenceRuleInjectingProbabilisticTheoremProver(
          vsf1,
          new TopRuleWeighter(
            new RankingRuleWeighter(
              new AwithCtxCwithCtxVecspaceRuleWeighter(
                new SimpleCompositeVectorMaker()))),
      new TypeConvertingPTP(
        new BoxerExpressionInterpreter[FolExpression] {
          def interpret(x: BoxerExpression): FolExpression =
            new Boxer2DrtExpressionInterpreter().interpret(
              new OccurrenceMarkingBoxerExpressionInterpreterDecorator().interpret(
                new MergingBoxerExpressionInterpreterDecorator().interpret(
                  new UnnecessarySubboxRemovingBoxerExpressionInterpreter().interpret(
                    new PredicateCleaningBoxerExpressionInterpreterDecorator().interpret(x))))).fol
        },
            new FakeProbabilisticTheoremProver(
              //              new TheoremProver[FolExpression, String] {
              //                def prove(assumptions: List[FolExpression], goal: Option[FolExpression] = None, verbose: Boolean = false): Option[String] = {
              //                  assumptions.map(println)
              //                  println
              //                  println(goal)
              //                  println
              //                  Option("...")
              //                }
              //              }))))

              new Prover9TheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/LADR-2009-11A/bin/prover9"), 5, false)))))

    //
    //
    //

    for (List(txt, hyp) <- a.grouped(2)) {
      println(txt.mkString(" "))
      println(hyp.mkString(" "))
      println(ttp.prove(txt, hyp))
    }

    //    def mtpo = new ModalTheoremProver(tpo)
    //    def vtpo = new VisualizingModalTheoremProverDecorator(mtpo)
    //    def vwtpo = new VisualizingModalTheoremProverDecorator(new InferenceRuleInjectingProbabilisticTheoremProver(mtpo))

  }
}


class Baseline (delegate: ProbabilisticTheoremProver[BoxerExpression])
	extends ProbabilisticTheoremProver[BoxerExpression] 
{
	private val LOG = LogFactory.getLog(classOf[Baseline])
	override def prove(
		constants: Map[String, Set[String]],
		declarations: Map[BoxerExpression, Seq[String]],
		evidence: List[BoxerExpression],
		assumptions: List[WeightedExpression[BoxerExpression]],
		goal: BoxerExpression): Seq[Double] = 
	{
		if (Sts.opts.baseline == "dep")
		{
			val textPreds = assumptions.head.expression.getPredicates.groupBy(x => x.name + "#" + x.variable + "#" + x.pos).map(_._2.head)
			val textRels = assumptions.head.expression.getRelations.groupBy(x => x.name + "#" + x.event + "#" + x.variable).map(_._2.head)
			val hypPreds = goal.getPredicates.groupBy(x => x.name + "#" + x.variable + "#" + x.pos).map(_._2.head)
			val hypRels = goal.getRelations.groupBy(x => x.name + "#" + x.event + "#" + x.variable).map(_._2.head)

			val textEntities = (textPreds.map(_.variable.name) ++ textRels.flatMap(r => List(r.variable.name, r.event.name))).toSet.toList
			val textEntitiesMap = textEntities.map(e => (e -> textPreds.filter(_.variable.name == e).toSet)).toMap

			val hypEntities = (hypPreds.map(_.variable.name) ++ hypRels.flatMap(r => List(r.variable.name, r.event.name))).toSet.toList
			val hypEntitiesMap = hypEntities.map(e => (e -> hypPreds.filter(_.variable.name == e).toSet)).toMap

			//entities extracted from predicates should contain all entities in relations too
			textRels.foreach(r => {
				require(textEntities.contains(r.event.name), "Entity 1 %s is not in text %s".format(r.event, textEntities));
				require(textEntities.contains(r.variable.name), "Entity 2 %s is not in text %s".format(r.variable, textEntities));
			})
			hypRels.foreach(r => {
				require(hypEntities.contains(r.event.name), "Entity %s is not in hypothesis  %s".format(r.event, hypEntities));
				require(hypEntities.contains(r.variable.name), "Entity %s is not in hypothesis %s".format(r.variable, hypEntities));
			})

			//find potential matched entities 
			val entityPotentialMatchs = hypEntities.map(hypE => (hypE -> textEntities.filter(textE => {
				//two entities match if they share a predicate with the same lemma
				val textWords = textEntitiesMap(textE).map(_.name)
				val hypWords = hypEntitiesMap(hypE).map(_.name)
				var x = textWords intersect hypWords
				x = x -- Set("male", "female", "topic") //ignore meta words
				//if (hypWords != tmp)
				//  tmp = hypWords
				var y = Set[String]();

				if (hypWords.contains("@placeholder"))
					y = Sts.qaEntities.keySet intersect textWords
				//println (hypWords.toString + " --- " + textWords.toString + " --- " + x.toString)
				!(x.isEmpty && y.isEmpty)
				//TODO: add potential matches using WordNet, DistSim and LexicalEnt
			}))).toMap

			//println(entityPotentialMatchs)
			val rules: ListBuffer[(BoxerDrs, BoxerDrs, Double, RuleType.Value)] = ListBuffer();

			val hypGraph = scalax.collection.mutable.Graph[String, LUnDiEdge]();
			hypEntities.foreach(e => hypGraph += e)
			hypRels.foreach(r => {
				val edge = (r.event.name ~+ r.variable.name)(r);
				//ignore this check, but still need to ckeck that the code below will still work without it
				//require( !hypGraph.contains(edge), "Edge " + edge + " already exists in graph " + hypGraph)
				hypGraph += edge
			});

			val textGraph = scalax.collection.mutable.Graph[String, LUnDiEdge]();
			textEntities.foreach(e => textGraph += e)
			textRels.foreach(r => {
				val edge = (r.event.name ~+ r.variable.name)(r);
				//Ignore this check because it is wrong, and because the code below will still work without it.
				//require( !textGraph.contains(edge), "Edge " + edge + " already exists in graph " + textGraph)
				textGraph += edge
			});
			
			val words:List[String] = null;
			val qWords:List[String] = null;
			var maxScore = 0.0
			var maxScoreEntity = "";
			val placeholderNode = hypGraph.get( hypPreds.filter ( _.name == "@placeholder" ).head.variable.name )

			Sts.qaEntities/*.filter ( e => e._2 != "" && e._1 != "@placeholder" )*/.foreach(ent=>
			{
				val textEntityInstances = textPreds.filter ( _.name == ent._1)
				var entityScore = 0.0;
				hypGraph.nodes.foreach(hypNode => 
				{
					val textWords = entityPotentialMatchs(hypNode.toString)
					var maxWordScore = 0.0;
					val hypSp = (hypNode shortestPathTo placeholderNode)
					if (!hypSp.isEmpty)
					{
						val hypDist =  hypSp.get.weight
						textEntityInstances.foreach(te => 
						{
							val entityNode = textGraph.get(te.variable.name)
							textWords.foreach(textWord => {
								val textSp = textGraph.get(textWord) shortestPathTo entityNode
								if (!textSp.isEmpty)
									maxWordScore = Math.max(maxWordScore, 1.0/(1+Math.abs(textSp.get.weight - hypDist)))
									
							})
						})
						//LOG.trace(" >> " + hypEntitiesMap(hypNode).map(_.name).mkString(", ") + " " + maxWordScore)
						entityScore = entityScore + maxWordScore
					}
				})

				LOG.info("## " + ent._1 + " " + entityScore)
				if( entityScore > maxScore)
				{
					maxScore = entityScore
					maxScoreEntity = ent._1
				}
			})
			LOG.info(">> " +  maxScoreEntity + " - " + maxScoreEntity + " - " + maxScore)
			//val matchingEnt = Sts.qaEntities.filter(_._2 == "h" + maxScoreEntity).toList
			//maxScoreEntity = matchingEnt.head._1 //use the first. UGLY C&C AND BOXER :@
			Sts.qaAnswer = maxScoreEntity
			return Seq(-7.0);
		}
		else 
			return delegate.prove(constants, declarations, evidence, assumptions, goal)
	}
}