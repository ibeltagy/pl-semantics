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
import utcompling.mlnsemantics.wordnet.Wordnet

class AlchemyTheoremProver(
  wordnet: Wordnet,
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
    goal: FolExpression): Option[Double] = {

    declarations.foreach { dec =>
      dec match {
        case (FolAtom(Variable(pred), args @ _*), argTypes) =>
          for (a <- argTypes)
            require(constants.contains(a), "No contants were found for type '%s' of declared predicate '%s'.".format(a, pred))
        case d => throw new RuntimeException("Only atoms may be declared.  '%s' is not an atom.".format(d))
      }
    }

    val declarationNames =
      declarations.mapKeys {
        case FolAtom(Variable(pred), _*) => pred
        case FolVariableExpression(Variable(pred)) => pred
      }

	/*var predsInRules = assumptions.map{
				case SoftWeightedExpression(folEx, weight) => 
				{
					if(weight.isNaN) ""
					else folEx.pretty
				}
          			case HardWeightedExpression(folEx) => folEx.pretty
        		}
	*/
	//We do not generate rules between predicates having the same name
	//This function generate rules using WordNet. This is removed for now
	//val samePredRule = createSamePredRule(declarationNames)
    val samePredRule = "";
	//if(samePredRule != "") predsInRules :+= samePredRule

    val mlnFile = makeMlnFile(
      constants,
      declarationNames,
      assumptions,
      evidence,
      goal,
      samePredRule)
					
    //val evidenceFile = makeEvidenceFile(evidence, predsInRules)
    val evidenceFile = makeEvidenceFile(evidence, List())
    val emptyEvidenceFile = makeEvidenceFile(List[FolExpression](), List[String]())
    val resultFile = FileUtils.mktemp(suffix = ".res")

    //Adding all predicates to the query force them to be open world
    //This could be slower but, this is the only way to set the predicates to open-world in 
    //Alchemy 2.0 because they removed this option
    //val args = List("-ow", declarationNames.keys.mkString(","), "-q", "entailment")
    //val args = List( "-q", declarationNames.keys.mkString(","))
    
    //all evd are in the mln file
    val args = Sts.opts.task match {
      case "sts" => List("-q", "entailment_h,entailment_t")
      case "rte" => List("-q", "entailment_h")
    }
    try 
    {      
    	callAlchemy(mlnFile, evidenceFile, resultFile, args) match {
	      case Some(t) => Some(t.toDouble)
	      case _ => throw new RuntimeException("no valid output in the result file");
    	}

    }catch 
    {
    	case e: Exception =>{
    	  System.err.println (e);
    	  return None;
    	}   				 
    }
    
  }

	/**
 	 * Generate inference rules for each pair of predicates having the same name. 
 	 * E.g., man_dt(indv0) => man_dh(indv0).
 	 */
	private def createSamePredRule(declarationNames: Map[String, Seq[String]]): String =
	{
		var rules = ""
		val (predsInText, predsInHypo) = declarationNames.partition
		{
			case (pred, _) =>
				if (pred.contains("_dt")) true
				else false
		}
	
		predsInHypo.foreach
		{
			case (pred_dh, varTypes_dh) =>
			{
				if(varTypes_dh.size == 1)
				{
					val varTypesH = varTypes_dh.mkString("").replaceAll("_[t|h]", "0")
					predsInText.foreach
					{
						case (pred_dt, varTypes_dt) if (varTypes_dt.size == 1) =>
						{
							val varTypesT = varTypes_dt.mkString("").replaceAll("_[t|h]", "0")

							val assumePred = "_" + pred_dt
							val goalPred = "_" + pred_dh

							var isValidRule = false
						
							if((assumePred.contains("_card_") || assumePred.contains("_time_") )
								&& varTypesT == varTypesH)
							{
								if(pred_dt.replaceAll("_dt", "") == pred_dh.replaceAll("_dh", ""))
									isValidRule = true
							}

							//else if(pred_dt.replaceAll("_[^_]*_dt", "") == pred_dh.replaceAll("_[^_]*_dh", ""))
							//	isValidRule = true

							else if (assumePred.contains("_" + pred_dh.replaceAll("_[^_]*_dh", "")) )
								isValidRule = true

							// Check if two words are similar via WordNet
							else
							{
								val assumeWord = pred_dt.replaceAll("_[^_]*_dt", "")
								val goalWord = pred_dh.replaceAll("_[^_]*_dh", "")
								val pos = if (pred_dt.contains("_v_")) "v"
									else if (pred_dt.contains("_a_")) "a"
									else "n"
								val similarWords = getSynonyms(assumeWord, pos) ++
											 getHypernyms(assumeWord, pos) ++
											getHyponyms(assumeWord, pos)

								if(similarWords.contains(goalWord)) isValidRule = true
									
							}

							if(isValidRule && varTypesH != "entail")
								rules += pred_dt + "(" + varTypesT + ") => " + 
									pred_dh + "(" + varTypesH + ").\n"
						}
						case _ =>
					}
				}
				
				// relation predicates
				else if(varTypes_dh.size == 2)
				{
					val varTypesH = varTypes_dh(0).mkString("").replaceAll("_[t|h]", "0") +
							"," +
							varTypes_dh(1).mkString("").replaceAll("_[t|h]", "1")

					predsInText.foreach
					{
						case (pred_dt, varTypes_dt) if (varTypes_dt.size == 2)  =>
						{
							val varTypesT = varTypes_dt(0).mkString("").replaceAll("_[t|h]", "0") +
									"," +
									varTypes_dt(1).mkString("").replaceAll("_[t|h]", "1")
						
							if (pred_dh.replaceAll("_[^_]*_dh", "") == pred_dt.replaceAll("_[^_]*_dt", "") )
								rules += pred_dt + "(" + varTypesT + ") => " + 
									pred_dh + "(" + varTypesH + ").\n"
						}
						case _ =>
					}
				}
			}
		}

		rules
	}

  private def makeMlnFile(
    constants: Map[String, Set[String]],
    declarationNames: Map[String, Seq[String]],
    assumptions: List[WeightedFolEx],
    evidence: List[FolExpression],
    goal: FolExpression,
    samePredRule: String) = {
    
    val tempFile = FileUtils.mktemp(suffix = ".mln")
    FileUtils.writeUsing(tempFile) { f =>
    constants.foreach {
    case (name, tokens) => f.write("%s = {%s}\n".format(name, tokens.map(quote).mkString(",")))
    }
    f.write("\n")

	declarationNames.foreach {
    		case (pred, varTypes) => f.write("%s(%s)\n".format(pred, varTypes.mkString(",")))
    }
    f.write("\n")

    f.write("\n//begin assumptions\n")

		assumptions
        .flatMap {
          case e @ SoftWeightedExpression(folEx, weight) =>
            weight match {
              case Double.PositiveInfinity => Some(HardWeightedExpression(folEx))
              case Double.NegativeInfinity => None ;//Some(HardWeightedExpression(-folEx))
              case _ if weight < Sts.opts.weightThreshold => None
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
	            
          case HardWeightedExpression(folEx) => f.write(insertExistQuant(convert(folEx)+ ".\n"))
        }
          
	    // Add same name predicates inference rules
	    if(samePredRule != "")
	    	f.write(samePredRule)
       }

      f.write("\n")
      //f.write(handwrittenRules);

    }
    tempFile
  }

  private def universalifyGoalFormula(goalFormula: FolIfExpression) = {
    val FolIfExpression(goal, consequent) = goalFormula

    def isConjoinedAtoms(e: FolExpression): Boolean = {
      e match {
        case FolAtom(_, _*) => true
        case FolAndExpression(a, b) => isConjoinedAtoms(a) && isConjoinedAtoms(b)
        case _ => false
      }
    }

    def universalify(e: FolExpression): FolExpression = {
      e match {
        case FolExistsExpression(v, term) => FolAllExpression(v, universalify(term))
        case _ if isConjoinedAtoms(e) => e -> consequent
        case _ => e -> consequent // sys.error(e.toString)
      }
    }

    universalify(goal)
  }

  private def makeEvidenceFile(evidence: List[FolExpression], predsInRules: List[String]) = {
    val tempFile = FileUtils.mktemp(suffix = ".db")
    FileUtils.writeUsing(tempFile) { f =>
      f.write("//\n");
      evidence.foreach {
        case e @ FolAtom(pred, args @ _*) => { 
						// Remove all evidence predicates that don't appear in inference rules.
						// This can help to reduce domain size.
						//val predName = pred.name.replace("'", "")
						//var isUsedAsEvidence = false
						//predsInRules.foreach(x => if (x.contains(predName)) isUsedAsEvidence = true)
						//if(isUsedAsEvidence) f.write(convert(e) + "\n")
						//else f.write("")
						f.write (convert(e) + "\n");
					}
        case e => throw new RuntimeException("Only atoms may be evidence.  '%s' is not an atom.".format(e))
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

    val (exitcode, stdout, stderr) = callAllReturns(None, allArgs, LOG.isTraceEnabled, Sts.opts.timeout);
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
      case "sts" => (score1 + score2) / 2.0;
      case "rte" => score1;
    }  
    
    //println(out);
    if (LOG.isDebugEnabled())
    {
    	val results = readLines(result).mkString("\n").trim
    	LOG.debug("results file:\n" + results)
    }

    exitcode match {
      case 0 => Some(score.toString())
      case _ => throw new RuntimeException("Failed with exitcode=%s.\n%s\n%s".format(exitcode, stdout, stderr))
    }
  }
  
  //find outer most variables in the exist clauses
  private def findVars(input: FolExpression, bound: Set[Variable] = Set()): Set[Variable] =
	input match {
	  case FolExistsExpression(variable, term) => findVars(term, bound+variable)
	  case _ => bound
  }
  
  //convert a FOLExpression to an Alchemy string 
  private def convert(input: FolExpression, bound: Set[Variable] = Set()): String =
    input match {
      case FolAllExpression(variable, term) => convert(term, bound + variable) // don't add outermost 'forall'
      case _ => _convert(input, bound)
    }

  private def _convert(input: FolExpression, bound: Set[Variable]): String =
    input match {
      case FolExistsExpression(variable, term) => "exist " + variable.name + " (" + _convert(term, bound + variable) + ")"
      case FolAllExpression(variable, term) => "(forall " + variable.name + " (" + _convert(term, bound + variable) + "))"
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

  // Find and insert existential quantifiers in the inference rule
  private def insertExistQuant(strFomular: String): String =
  {
	val rhsVars = strFomular.replaceAll(",", """\)\(""")
		.split("\\(")
		.filter(_.startsWith("rhs_"))
		.map(_.split("\\)")(0))
		.distinct

	strFomular // We have problems with existential quantifiers. So, using universal quantifiers instead.

	//if (!rhsVars.isEmpty) "exist " + rhsVars.mkString(",") + " " + strFomular
	//else strFomular
  }

  private def getSynonyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- wordnet.synsets(name, p);
      w <- s.getWords
    ) yield w.getLemma).toSet + name -- Set("POS", "NEG") //TODO: REMOVE THE "+ name".  WE ONLY WANT NEED THIS FOR WHEN THE WORD ISN'T IN WORDNET.

  private def getHypernyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- wordnet.synsets(name, p);
      h <- wordnet.allHypernyms(s, 20);
      w <- h.getWords
    ) yield w.getLemma).toSet

  private def getHyponyms(name: String, pos: String): Set[String] =
    (for (
      p <- getPos(pos);
      s <- wordnet.synsets(name, p);
      h <- wordnet.allHyponyms(s, 20);
      w <- h.getWords
    ) yield w.getLemma).toSet

  private def getPos(s: String) =
    s match {
      case "n" => List(POS.NOUN)
      case "v" => List(POS.VERB)
      case "a" => List(POS.ADJECTIVE)
      case _ => Nil
    }

}

object AlchemyTheoremProver {

  private var pairIndx = 0;

  def findBinary(wordnet: Wordnet, binDir: Option[String] = None, envar: Option[String] = Some("ALCHEMYHOME"), verbose: Boolean = false) =
  {    //new AlchemyTheoremProver(FileUtils.findBinary("liftedinfer", binDir, envar, verbose))
	pairIndx = pairIndx+1;
	//println("pairIndx: " + pairIndx);
    new AlchemyTheoremProver(wordnet, FileUtils.findBinary("infer", binDir, envar, verbose))
  }

  def main(args: Array[String]) {
	import utcompling.mlnsemantics.wordnet.WordnetImpl

    val parse = new FolLogicParser().parse(_)

	val wordnet = new WordnetImpl()

    val atp = new AlchemyTheoremProver(wordnet, pathjoin(System.getenv("HOME"), "bin/alchemy/bin/infer"))

    val constants = Map("ind" -> Set("Socrates"))
    val declarations = Map[FolExpression, Seq[String]](FolAtom(Variable("man")) -> Seq("ind"), FolAtom(Variable("mortal")) -> Seq("ind"))
    val evidence = List("man(Socrates)").map(parse)
    val assumptions = List(HardWeightedExpression(parse("all x.(man(x) -> mortal(x))")))
    val goal = parse("mortal(Socrates)")
    println(atp.prove(constants, declarations, evidence, assumptions, goal))

  }
}
