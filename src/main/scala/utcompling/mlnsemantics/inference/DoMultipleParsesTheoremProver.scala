package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import scala.collection.mutable.SetBuilder
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.mlnsemantics.wordnet.Wordnet
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import org.apache.commons.logging.LogFactory
import support.HardWeightedExpression
import utcompling.mlnsemantics.run.Sts

class DoMultipleParsesTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {  
  
  private val LOG = LogFactory.getLog(classOf[DoMultipleParsesTheoremProver])

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {

    val kbest = Sts.opts.get("-kbest") match {
		case Some(cnt) => cnt.toInt;
		case _ => 1000;
    }
    
    val task = Sts.opts.get("-task") match {
		case Some(tsk) => tsk;
		case _ => "sts";
    }
    var score: Double = 0;
    var scoreDenum: Double = 0;
    goal match {
      case BoxerPrs(goalParses) => goalParses.slice(0, kbest).foreach(goalParse=>{  
    	assumptions.head.expression match{
    	  case BoxerPrs(assumptionParses) => assumptionParses.slice(0, kbest).foreach(assumptionParse=>{
    	  //---------------------------given goalParse and assumptionParse, calculate one score then add it to total score
    	  val result = delegate.prove(constants, declarations, evidence, List(HardWeightedExpression(assumptionParse._1)), goalParse._1)
    	  val oneScore = result match { case Some(s) => s; case None => 0.5};
    	  task match {
    	    case "sts" => score += oneScore*(assumptionParse._2+goalParse._2); scoreDenum +=(assumptionParse._2+goalParse._2 ); //weighted average 
    	    case "rte" => score = Math.max(score, oneScore); scoreDenum = 1; //max
    	   }
    	  //------------------------------  
    	  })
    	  case _ =>  throw new RuntimeException ("Premise and Hypothesis both should start with BoxerPrs")
    	}
      })
      case _ => return delegate.prove(constants, declarations, evidence, assumptions, goal)
    }
     return Some(score/scoreDenum);
  }  
}
