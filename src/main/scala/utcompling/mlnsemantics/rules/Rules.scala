package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.datagen.SimpleTokenizer
import utcompling.mlnsemantics.run.Sts
import utcompling.scalalogic.discourse.candc.boxer.expression._
import dhg.depparse.Lemmatize
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression

class Rules {
}

object Rules {

  private val LOG = LogFactory.getLog(classOf[Rules])

  /**
   * Convert paraphrase rules in text format to FOL.
   * Rules have the format: <id> TAB <text_phrase> TAB <hypo_phrase> TAB <sim_score>
   */
  def convertRulesToFOL(rules: List[String], text: BoxerExpression, hypothesis: BoxerExpression): List[(BoxerDrs, BoxerDrs, Double)] =
    {
      val assumePredsList = text.getPredicates
      val goalPredsList = hypothesis.getPredicates
      val assumeRelsList = text.getRelations
      val goalRelsList = hypothesis.getRelations

      val folRules = rules.flatMap { rule =>
        val Array(id, left, right, score) = rule.split("\t")

        val leftTokens = left.split(" ").flatMap(token => Array(token, Lemmatize(token, "")))
          .distinct

        // Find relating predicates for the lhs
        val matchAssumePreds = assumePredsList.filter { pred =>

          /*var isContained = true
				pred.name.split("_").foreach { token =>    //again, split is wrong
					if(token != "topic" && !leftTokens.contains(token) && !leftTokens.contains(token + "s")) 
						isContained = false
				}
				isContained*/
          (pred.name == "topic" || leftTokens.contains(pred.name) || leftTokens.contains(pred.name + "s"))
        }
        val matchAssumePredVars = matchAssumePreds.map(pred => pred.variable.name)
        val matchAssumeRels = assumeRelsList.filter(rel =>
          (matchAssumePredVars.contains(rel.event.name) && matchAssumePredVars.contains(rel.variable.name))
            || ((matchAssumePredVars.contains(rel.event.name) || matchAssumePredVars.contains(rel.variable.name)
              || matchAssumePredVars.isEmpty)
              && leftTokens.contains(rel.name)))

        val rightTokens = right.split(" ").flatMap(token => Array(token, Lemmatize(token, "")))
          .distinct

        // Find relating predicates for the rhs
        val matchGoalPreds = goalPredsList.filter { pred =>
          /*var isContained = true
				pred.name.split("_").foreach { token =>   //again, split here is wrong
					if(token != "topic" && !rightTokens.contains(token) && !rightTokens.contains(token + "s")) 
						isContained = false
				}
				isContained*/
          (pred.name == "topic" || rightTokens.contains(pred.name) || rightTokens.contains(pred.name + "s"))
        }
        val matchGoalPredVars = matchGoalPreds.map(pred => pred.variable.name)
        val matchGoalRels = goalRelsList.filter(rel =>
          (matchGoalPredVars.contains(rel.event.name) && matchGoalPredVars.contains(rel.variable.name))
            || ((matchGoalPredVars.contains(rel.event.name) || matchGoalPredVars.contains(rel.variable.name)
              || matchGoalPredVars.isEmpty)
              && rightTokens.contains(rel.name)))

        // Use this map to rename variables in the rhs
        var assumVarNameMap = Map[String, String]()
        var goalVarNameMap = Map[String, String]()

        var counter = 0;
        matchAssumePredVars.sortWith(_.compareTo(_) < 0).map(v => {
          assumVarNameMap += (v -> ("x" + counter.toString()));
          counter = counter + 1;
        })

        counter = 0;
        matchGoalPredVars.sortWith(_.compareTo(_) < 0).map(v => {
          goalVarNameMap += (v -> ("x" + counter.toString()));
          counter = counter + 1;
        })

        /*matchGoalPreds.map { pred =>()
				if(matchAssumePreds.size == 1 && matchGoalPreds.size == 1) 
					varNameMap += (pred.variable.name -> matchAssumePreds(0).variable.name) 
				else matchAssumePreds.foreach { assumePred =>
					val assumePredName = ("_" + assumePred.name + "_").replaceAll("_topic_", "_")
					val goalPredName = ("_" + pred.name + "_").replaceAll("_topic_", "_")
					if( (assumePred.toString.contains(",v,") && pred.toString.contains(",v,")) ||
						assumePredName.contains(goalPredName) || 
						goalPredName.contains(assumePredName)
					)
						varNameMap += (pred.variable.name -> assumePred.variable.name) 
				}
			}*/

        val changedMatchAssumPreds = matchAssumePreds.map { pred =>
          BoxerPred(pred.discId,
            pred.indices,
            BoxerVariable(assumVarNameMap.getOrElse(pred.variable.name, pred.variable.name)),
            pred.name,
            pred.pos,
            pred.sense)
        }
        val changedMatchAssumRels = matchAssumeRels.map { rel =>
          BoxerRel(rel.discId,
            rel.indices,
            BoxerVariable(assumVarNameMap.getOrElse(rel.event.name, rel.event.name)),
            BoxerVariable(assumVarNameMap.getOrElse(rel.variable.name, rel.variable.name)),
            rel.name,
            rel.sense)
        }
        ////////////////////////			

        val changedMatchGoalPreds = matchGoalPreds.map { pred =>
          BoxerPred(pred.discId,
            pred.indices,
            BoxerVariable(goalVarNameMap.getOrElse(pred.variable.name, pred.variable.name)),
            pred.name,
            pred.pos,
            pred.sense)
        }
        val changedMatchGoalRels = matchGoalRels.map { rel =>
          BoxerRel(rel.discId,
            rel.indices,
            BoxerVariable(goalVarNameMap.getOrElse(rel.event.name, rel.event.name)),
            BoxerVariable(goalVarNameMap.getOrElse(rel.variable.name, rel.variable.name)),
            rel.name,
            rel.sense)
        }

        val leftFOL = matchAssumePreds ++ matchAssumeRels
        val rightFOL = matchGoalPreds ++ matchGoalRels
        //val leftFOL = changedMatchAssumPreds ++ changedMatchAssumRels
        //val rightFOL = changedMatchGoalPreds ++ changedMatchGoalRels

        if (leftFOL.isEmpty || rightFOL.isEmpty)
          None
        else {

          val lhsDrs = BoxerDrs((matchAssumePredVars ++ matchGoalPredVars).map(v => (List() -> BoxerVariable(v))).toList, leftFOL.toList);
          val rhsDrs = BoxerDrs((matchAssumePredVars ++ matchGoalPredVars).map(v => (List() -> BoxerVariable(v))).toList, rightFOL.toList);
          
          List((lhsDrs, rhsDrs, score.toDouble))
        }
      }
      return folRules;
    }

  private def changeExpDirection(e: BoxerExpression): BoxerExpression =
    {
      e match {
        case BoxerRel(discId, indices, event, variable, name, sense) => BoxerRel(discId match { case "h" => "t"; case "t" => "h"; }, indices, event, variable, name, sense);
        case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred(discId match { case "h" => "t"; case "t" => "h"; }, indices, variable, name, pos, sense);
        case _ => e.visitConstruct(changeExpDirection);
      }
    }

  def createWeightedExpression(leftFOL: BoxerDrs, rightFOL: BoxerDrs, score: Double): List[WeightedExpression[BoxerExpression]] =
  {
    if(score < Sts.opts.weightThreshold )
      return List();
  
    List(createWeightedExpression(leftFOL, rightFOL, "h", score)).flatten ++
    (
      if (Sts.opts.task == "sts")
        List(createWeightedExpression(rightFOL, leftFOL, "t", score)).flatten
      else List())
  }

  private def createWeightedExpression(leftFOL: BoxerDrs, rightFOL: BoxerDrs, discId: String, score: Double): Option[WeightedExpression[BoxerExpression]] =
    {
      val lhsSet = leftFOL.refs.map(_._2).toSet
      val rhsSet = rightFOL.refs.map(_._2).toSet
      val diff = rhsSet -- lhsSet; 
      if( ! diff.isEmpty )
      {
        //println ("Rule ignored: " + leftFOL + " => " + rightFOL )
        return None //RHS has variables that are not in the LHS
      }

      val changedLHS = changeExpDirection(leftFOL);
      var lhs = changedLHS.asInstanceOf[BoxerDrs]();      
      var rhs = rightFOL
      val allRefs = (lhs.refs ++ rhs.refs).toSet.toList
      rhs = BoxerDrs(List(), rhs.conds);
      lhs = BoxerDrs(allRefs, lhs.conds);
      val unweightedRule = BoxerImp(discId, List(), lhs, rhs)
      return Some(SoftWeightedExpression(unweightedRule, score))
    }
}