package utcompling.mlnsemantics.inference

import org.apache.commons.logging.LogFactory
import scala.io.Source
import scala.math._
import utcompling.scalalogic.top.expression._
import utcompling.scalalogic.base.expression._
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import utcompling.scalalogic.util.SubprocessCallable
import utcompling.mlnsemantics.inference.support._
import java.util.StringTokenizer
import utcompling.mlnsemantics.run.Sts
import scala.sys.process.Process
import scala.sys.process.ProcessLogger
import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import scala.compat.Platform

class SampleSearchTheoremProver
  extends AlchemyTheoremProver 
  with ProbabilisticTheoremProver[FolExpression]{
  
  private val LOG = LogFactory.getLog(classOf[SampleSearchTheoremProver])
  
  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[FolExpression, Seq[String]],
    evidence: List[FolExpression],
    assumptions: List[WeightedFolEx],
    goal: FolExpression): Seq[Double] = {

	assert(Sts.opts.task == "rte"); //sts is not supported here
	
    var goals:List[WeightedFolEx] = List();
    
    val mlnFile = makeMlnFile(
      constants,
      declarations,
      assumptions.flatMap{
        case e @ GoalExpression(folExp, weight) => goals = goals :+ e; None;
        case e @ _ => Some(e)
      },
      evidence,
      goal)
					
    val evidenceFile = makeEvidenceFile(evidence)
    val resultFile = FileUtils.mktemp(suffix = ".res")
    val queryFile = makeQueryFile(goals);
    
   
    //all evd are in the mln file
    val openWorldPreds =
      declarations.keys.map{
        case FolAtom(Variable(pred), _*) => pred
        case FolVariableExpression(Variable(pred)) => pred
    }.mkString(",")
    
    val args = List("-ss", "-maxSeconds", "5", "-ssq", queryFile) ++ (Sts.opts.task match {
      case "sts" => List("-q", openWorldPreds)
      case "rte" => List("-q", openWorldPreds)
    })
   
  	callAlchemy(mlnFile, evidenceFile, resultFile, args) match {
  		case 0  =>
  		{
  			val results = readLines(resultFile).toList;
  			results.foreach(LOG.trace(_))
  			if(results.length != 1)
  			{
  			  Seq(-1);
  			}
  			else
  			{
	  			val score = results(0).toDouble;
	  			Seq(score);
  			}
	      }
	      case -3 => Seq(-3.0); 
	      case x  => Seq(-1.0); 
		}    
  }

  private def makeQueryFile(goals: List[WeightedFolEx]) = {
    val tempFile = FileUtils.mktemp(suffix = ".q");    
    FileUtils.writeUsing(tempFile) { f =>
      goals.foreach {
	          case GoalExpression(folExp, weight) =>
	          {
	        	  if(weight == Double.PositiveInfinity)
	        		  f.write("%s .\n".format(convert(folExp)))
	        	  else
	        	      f.write("%.5f %s\n".format(weight, convert(folExp)))  
	          }
	          case _  => assert(false);
       }
    }
    tempFile
  }
  
}

object SampleSearchTheoremProver {
  
}
