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

class MLNTheoremProver
extends AlchemyTheoremProver 
with ProbabilisticTheoremProver[FolExpression]{

  private val LOG = LogFactory.getLog(classOf[MLNTheoremProver])
  
  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[FolExpression, Seq[String]],
    evidence: List[FolExpression],
    assumptions: List[WeightedFolEx],
    goal: FolExpression): Seq[Double] = {


    val mlnFile = makeMlnFile(
      constants,
      declarations,
      assumptions,
      evidence,
      goal)
					
    val evidenceFile = makeEvidenceFile(evidence)
    val resultFile = FileUtils.mktemp(suffix = ".res")
   
    //all evd are in the mln file
    val openWorldPreds =
      declarations.keys.map{
        case FolAtom(Variable(pred), _*) => pred
        case FolVariableExpression(Variable(pred)) => pred
    }.mkString(",")
    
    val args = List("-ow", openWorldPreds) ++ (Sts.opts.task match {
      case "sts" => List("-q", "entailment_h,entailment_t")
      case "rte" => List("-q", "entailment_h")
    })

     
	val result = callAlchemy(mlnFile, evidenceFile, resultFile, args) match {
      case 0  =>
      {
		    val out = new StringBuilder
			val err = new StringBuilder
		 
		    /*var command = (Sts.opts.task == "sts" && varBind.get) match {
					case true =>  "grep entailment_h "+result+" | awk '{print $2}'| sort -n -r  | head -n 1";
					case false => "grep \"entailment_h(\\\"ent_h\" "+result+" | awk '{print $2}'| sort -n -r  | head -n 1"
			 }*/
			var command = "grep entailment_h " + resultFile + " | awk '{print $2}'| sort -n -r  | head -n 1";
			
		    Process("/bin/sh", Seq("-c", command)) ! (ProcessLogger(out.append(_).append("\n"), System.err.println(_)))
		    
		    var score1 = 0.0;
		    try {
		      score1 = out.mkString("").trim().toDouble;
		    }catch {case _ => score1 = - 1;}
		    out.clear();
		    
			 var score2 = 0.0;
			 if (Sts.opts.task == "sts")
			 {
						/*command = varBind.get match {
							  case true =>  "grep entailment_t "+result+" | awk '{print $2}'| sort -n -r  | head -n 1";
							  case false => "grep \"entailment_t(\\\"ent_t\" "+result+" | awk '{print $2}'| sort -n -r  | head -n 1"
						}*/
			   			command = "grep entailment_t " + resultFile + " | awk '{print $2}'| sort -n -r  | head -n 1";
		
						Process("/bin/sh", Seq("-c", command)) ! (ProcessLogger(out.append(_).append("\n"), System.err.println(_)))
						
					    try {
					      score2 = out.mkString("").trim().toDouble;
					    }catch {case _ => score2 = - 1;}
						out.clear();
			 }
		    
 		     if (LOG.isDebugEnabled())
		     {
		    	val results = readLines(resultFile).mkString("\n").trim
		    	LOG.debug("results file:\n" + results)
		     }
 		     val score  = Sts.opts.task match {
		      case "sts" => Seq(score1, score2);
		      case "rte" => Seq(score1);
		     }
 		     score;
      }
      case -3 => Sts.opts.task match {
		      			case "sts" => Seq(-3.0, -3.0);
		      			case "rte" => Seq(-3.0);
      				} 
      case x  => Sts.opts.task match {
		      			case "sts" => Seq(-1.0, -1.0);
		      			case "rte" => Seq(-1.0);
      				} 
	} 

    if (!LOG.isTraceEnabled())
    {
    	FileUtils.remove(mlnFile)
    	FileUtils.remove(evidenceFile)
    	FileUtils.remove(resultFile)
    }
    return result
  }
}

object MLNTheoremProver {

  private var pairIndx = 0;


  def main(args: Array[String]) {
    val parse = new FolLogicParser().parse(_)
    val atp = new MLNTheoremProver
    val constants = Map("ind" -> Set("Socrates"))
    val declarations = Map[FolExpression, Seq[String]](FolAtom(Variable("man")) -> Seq("ind"), FolAtom(Variable("mortal")) -> Seq("ind"))
    val evidence = List("man(Socrates)").map(parse)
    val assumptions = List(HardWeightedExpression(parse("all x.(man(x) -> mortal(x))")))
    val goal = parse("mortal(Socrates)")
    println(atp.prove(constants, declarations, evidence, assumptions, goal))

  }
}
