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

class AlchemyTheoremProver{
  
  type WeightedFolEx = WeightedExpression[FolExpression]

  private val LOG = LogFactory.getLog(classOf[AlchemyTheoremProver])
  
  private val binary:String = FileUtils.findBinary("infer", Some("alchemy/bin") , Some("ALCHEMYHOME"))
  
  protected def makeMlnFile(
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

  protected def makeEvidenceFile(evidence: List[FolExpression]) = {
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

  protected def callAlchemy(mln: String, evidence: String, result: String, args: List[String] = List()): Int = {
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
    var caller = new SubprocessCallable(binary);
    val (exitcode, stdout, stderr) = caller.callAllReturns(None, allArgs, LOG.isTraceEnabled, Sts.opts.timeout);
    val tEnd = Platform.currentTime;
    //println("end time %s".format(tEnd));
	 println ("Total time:(%s) %s".format(SetVarBindPTP.varBind, (tEnd-tStart)/1000.0));

     return exitcode;
  }
  
  //convert a FOLExpression to an Alchemy string 
  protected def convert(input: FolExpression, bound: Set[Variable] = Set()): String =
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

  protected def quote(s: String) = '"' + s + '"'
}

object AlchemyTheoremProver {
  
}
