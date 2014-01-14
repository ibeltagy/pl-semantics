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

class AlchemyTheoremProver(
  override val binary: String,
  logBase: Double = E)
  extends SubprocessCallable(binary)
  with ProbabilisticTheoremProver[FolExpression] {

  type WeightedFolEx = WeightedExpression[FolExpression]

  private val LOG = LogFactory.getLog(classOf[AlchemyTheoremProver])
  
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
    
    val args = /*List("-ow", openWorldPreds) ++ */(Sts.opts.task match {
      case "sts" => List("-q", openWorldPreds)
      case "rte" => List("-q", openWorldPreds)
    })
    try 
    {      
    	callAlchemy(mlnFile, evidenceFile, resultFile, args) match {
	      case Some(t) => Seq(t.toDouble)
	      case _ => throw new RuntimeException("no valid output in the result file");
    	}

    }catch 
    {
    	case e: Exception =>{
    	  System.err.println (e);
    	  return Seq();
    	}   				 
    }
    
  }

  private def makeMlnFile(
    constants: Map[String, Set[String]],
    declarations: Map[FolExpression, Seq[String]],
    assumptions: List[WeightedFolEx],
    evidence: List[FolExpression],
    goal: FolExpression) = {
    
    declarations.foreach { dec =>
      dec match {
        case (FolAtom(Variable(pred), args @ _*), argTypes) =>
          for (a <- argTypes)
			 {
            require(constants.contains(a), "No contants were found for type '%s' of declared predicate '%s'.".format(a, pred))
				if(!constants.contains(a))
					LOG.error ("No contants were found for type '%s' of declared predicate '%s'.".format(a, pred));
					//return Some(-1.0);
			 }
        case d => {
				throw new RuntimeException("Only atoms may be declared.  '%s' is not an atom.".format(d))
				//LOG.error("Only atoms may be declared.  '%s' is not an atom.".format(d));
				//return Some(-1.0);
			}
      }
    }

    val declarationNames =
      declarations.mapKeys {
        case FolAtom(Variable(pred), _*) => pred
        case FolVariableExpression(Variable(pred)) => pred
      }

    val tempFile = FileUtils.mktemp(suffix = ".mln")
    FileUtils.writeUsing(tempFile) { f =>
    constants.foreach {
    	case (name, tokens) => {
    	  if (tokens.size == 0)
    		  f.write("%s = {%s}\n".format(name, quote(name+"_default")))
    	  else
    		  f.write("%s = {%s}\n".format(name, tokens.map(quote).mkString(",")))
    	}
    }
    f.write("\n")

	declarationNames.foreach {
    		case (pred, varTypes) => f.write("%s(%s)\n".format(pred, varTypes.mkString(",")))
    }
    f.write("\n")

    evidence.foreach {
        case e @ FolAtom(pred, args @ _*) if (!pred.name.startsWith("skolem_")) => f.write (convert(e) + ".\n");
        case e @ FolAtom(pred, args @ _*) => ;
        case e @ FolNegatedExpression(FolAtom(pred, args @ _*)) => f.write (convert(e) + ".\n");
        case e => throw new RuntimeException("Only atoms or negated atoms may be evidence.  '%s' is not an atom.".format(e))
      }
    
    f.write("\n//begin assumptions\n")

		assumptions
        .flatMap {
          case e @ SoftWeightedExpression(folEx, weight) =>
            weight match {
              case Double.PositiveInfinity => Some(HardWeightedExpression(folEx))
              case Double.NegativeInfinity => None ;//Some(HardWeightedExpression(-folEx))
              case _ => Some(e)
            }
          case e @ _ => Some(e)
        }.foreach { e => 
          e match {
	          case PriorExpression(folExp, weight) => 
	            	f.write("%.5f %s\n".format(weight, convert(folExp)))
	          case GoalExpression(folExp, weight) =>
	          {
	        	  if(weight == Double.PositiveInfinity)
	        		  f.write("%s .\n".format(convert(folExp)))
	        	  else
	        	      f.write("%.5f %s\n".format(weight, convert(folExp)))  
	          }
	          case SoftWeightedExpression(folExp, weight) =>
	            // DONE: Convert [0,1] weight into alchemy weight
	            //            val usedWeight = log(weight / (1 - weight)) / log(logBase) // treat 'weight' as a prob and find the log-odds
	            //            f.write(usedWeight + " " + convert(folEx) + "\n")
	            val folExpString = convert(folExp);
            	var usedWeight = min(weight, 0.999);
	            usedWeight = max(usedWeight, 0.001);
	            usedWeight = SetPriorPTP.predPrior + log(usedWeight) - log(1-usedWeight);
	            if (usedWeight  > 0)
	            {
	              //This is a nasty hack to inverse what alchamy does when it splits a formula into smaller formulas
	              var count = folExpString.split("=>").apply(1).count(_ == '^') + 1;
	              if (!Sts.opts.scaleW) 
				    count = 1;
	              usedWeight = usedWeight * count;
	              f.write("%.5f %s\n".format(usedWeight, folExpString))
	            }

	            //val usedWeight = 10 * weight // 5 * (pow(weight, 10)) //DONE: Set these parameters!!
	            // DONE: we want to design a function `f` such that, for the simplest examples (only one weighted clause), mln(f(s)) == s
	            //   meaning that the probability of entailment (`mln`) using a weight `f(s)` based on similarity score `s <- [0,1]` will be
	            //   roughly equal to the similarity score itself.
	            
          case HardWeightedExpression(folEx) => f.write(convert(folEx)+ ".\n")
        }
          
       }
    }
    tempFile
  }

  private def makeEvidenceFile(evidence: List[FolExpression]) = {
    val tempFile = FileUtils.mktemp(suffix = ".db")
    FileUtils.writeUsing(tempFile) { f =>
      f.write("//\n");
      evidence.foreach {
        case e @ FolAtom(pred, args @ _*) if (pred.name.startsWith("skolem_"))=> f.write (convert(e) + "\n");
        case e => //throw new RuntimeException("Only atoms may be evidence.  '%s' is not an atom.".format(e))
      }
    }
    tempFile
  }

  private def callAlchemy(mln: String, evidence: String, result: String, args: List[String] = List()): Option[String] = {
    if (LOG.isDebugEnabled) {
      LOG.debug("mln file:\n" + readLines(mln).mkString("\n").trim)
      LOG.debug("evidence file:\n" + readLines(evidence).mkString("\n").trim)
    }    

    //lifted belife propagation works better
    //val allArgs = "-bp" :: "-lifted" :: "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    //Dunno, but it seems that MC-SAT is better
    //val allArgs = "-ptpe" :: "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    val allArgs = "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    val tStart = Platform.currentTime;
	 //println("start time %s".format(tStart));
    val (exitcode, stdout, stderr) = callAllReturns(None, allArgs, LOG.isTraceEnabled, Sts.opts.timeout);
    val tEnd = Platform.currentTime;
    //println("end time %s".format(tEnd));
	 println ("Total time:(%s) %s".format(SetVarBindPTP.varBind, (tEnd-tStart)/1000.0));

    exitcode match {
      case 0 => 
      {
		    val out = new StringBuilder
			val err = new StringBuilder
		 
		    /*var command = (Sts.opts.task == "sts" && varBind.get) match {
					case true =>  "grep entailment_h "+result+" | awk '{print $2}'| sort -n -r  | head -n 1";
					case false => "grep \"entailment_h(\\\"ent_h\" "+result+" | awk '{print $2}'| sort -n -r  | head -n 1"
			 }*/
			var command = "grep entailment_h "+result+" | awk '{print $2}'| sort -n -r  | head -n 1";
			
		    Process("/bin/sh", Seq("-c", command)) ! (ProcessLogger(out.append(_).append("\n"), System.err.println(_)))
		    
		    val score1 = out.mkString("").trim().toDouble;
		    out.clear();
		    
			 var score2 = 0.0;
			 if (Sts.opts.task == "sts")
			 {
						/*command = varBind.get match {
							  case true =>  "grep entailment_t "+result+" | awk '{print $2}'| sort -n -r  | head -n 1";
							  case false => "grep \"entailment_t(\\\"ent_t\" "+result+" | awk '{print $2}'| sort -n -r  | head -n 1"
						}*/
			   			command = "grep entailment_t "+result+" | awk '{print $2}'| sort -n -r  | head -n 1";
		
						Process("/bin/sh", Seq("-c", command)) ! (ProcessLogger(out.append(_).append("\n"), System.err.println(_)))
						
						score2 = out.mkString("").trim().toDouble;
						out.clear();
			 }
		    
		    val score  = Sts.opts.task match {
		      case "sts" => (score1*100000 + score2);
		      case "rte" => score1;
		    }  
		
		    if (LOG.isDebugEnabled())
		    {
		    	val results = readLines(result).mkString("\n").trim
		    	LOG.debug("results file:\n" + results)
		    }
		    Some(score.toString())
      }
      //case _ => throw new RuntimeException("Failed with exitcode=%s.\n%s\n%s".format(exitcode, stdout, stderr))
      case -3 /*timeout*/ => Some("-3")
      case _ => throw new RuntimeException("Failed with exitcode=%s.".format(exitcode))
    }
  }
  
  //convert a FOLExpression to an Alchemy string 
  private def convert(input: FolExpression, bound: Set[Variable] = Set()): String =
    input match {
      case FolAllExpression(variable, term) => convert(term, bound + variable) // don't add outermost 'forall'
      case _ => _convert(input, bound)
    }

  private def _convert(input: FolExpression, bound: Set[Variable], outer:FolExpression = null): String =
    input match {
      case FolExistsExpression(variable, term) => {
    	  if (outer != null && outer.isInstanceOf[FolExistsExpression])
    	    ", " + variable.name + _convert(term, bound + variable, input)
    	  else
    		 "(exist " + variable.name + _convert(term, bound + variable, input) + ")" 
      }
      case FolAllExpression(variable, term) => {
       	  if (outer != null && outer.isInstanceOf[FolAllExpression])
    	    ", " + variable.name + _convert(term, bound + variable, input)
    	  else
    		 "(forall " + variable.name + _convert(term, bound + variable, input) + ")" 
      }
      case FolNegatedExpression(term) => "!(" + _convert(term, bound) + ")"
      case FolAndExpression(first, second) => "(" + _convert(first, bound) + " ^ " + _convert(second, bound) + ")"
      case FolOrExpression(first, second) => "(" + _convert(first, bound) + " v " + _convert(second, bound) + ")"
      case FolIfExpression(first, second) => "(" + _convert(first, bound) + " => " + _convert(second, bound) + ")"
      case FolIffExpression(first, second) => "(" + _convert(first, bound) + " <=> " + _convert(second, bound) + ")"
      case FolEqualityExpression(first, second) =>
        	"(" + _convert(first, bound) + " = " + _convert(second, bound) + ")";	
        	//both variables of the same type
	        /* This part is not needed anymore because we do not have separate types for Events and Indvs anymore. 
	        * if (first.asInstanceOf[FolVariableExpression].variable.name.charAt(1) == 
	        		second.asInstanceOf[FolVariableExpression].variable.name.charAt(1))
	        	"(" + _convert(first, bound) + " = " + _convert(second, bound) + ")";
	        else
	        	// dummy exp
	        	"(" + _convert(first, bound) + " = " + _convert(second, bound) + ")";
	        */
      case FolAtom(pred, args @ _*) => pred.name.replace("'", "") + "(" + args.map(v => if (bound(v)) v.name.toLowerCase() else quote(v.name)).mkString(",") + ")"
//      case FolVariableExpression(v) => if (bound(v)) v.name.toLowerCase() else quote(v.name)
	case FolVariableExpression(v) => v.name.toLowerCase()
    }

  private def quote(s: String) = '"' + s + '"'
}

object AlchemyTheoremProver {

  private var pairIndx = 0;

  def findBinary(binDir: Option[String] = Some("alchemy/bin"), envar: Option[String] = Some("ALCHEMYHOME"), verbose: Boolean = false) =
  {    //new AlchemyTheoremProver(FileUtils.findBinary("liftedinfer", binDir, envar, verbose))
	pairIndx = pairIndx+1;
	//println("pairIndx: " + pairIndx);
    new AlchemyTheoremProver(FileUtils.findBinary("infer", binDir, envar, verbose))
  }

  def main(args: Array[String]) {
    val parse = new FolLogicParser().parse(_)
    val atp = new AlchemyTheoremProver(pathjoin(System.getenv("HOME"), "bin/alchemy/bin/infer"))
    val constants = Map("ind" -> Set("Socrates"))
    val declarations = Map[FolExpression, Seq[String]](FolAtom(Variable("man")) -> Seq("ind"), FolAtom(Variable("mortal")) -> Seq("ind"))
    val evidence = List("man(Socrates)").map(parse)
    val assumptions = List(HardWeightedExpression(parse("all x.(man(x) -> mortal(x))")))
    val goal = parse("mortal(Socrates)")
    println(atp.prove(constants, declarations, evidence, assumptions, goal))

  }
}
