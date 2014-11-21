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

class TuffyTheoremProver(
  override val binary: String,
  prior: Double = -2.9,
  var entWeight: Double = 1,
  logBase: Double = E)
  extends SubprocessCallable(binary)
  with ProbabilisticTheoremProver[FolExpression] {

  type WeightedFolEx = WeightedExpression[FolExpression]

  private val LOG = LogFactory.getLog(classOf[TuffyTheoremProver])

  private val entailedConst = ("entail" -> Set("entailed"))
  private var entailedDec = FolVariableExpression(Variable("entailment")) -> Seq("entail")
  private var entailmentConsequent = FolAtom(Variable("entailment"), Variable("entailed")); //FolVariableExpression(Variable("entailment")) -> Seq("");//= 
  private var ResultsRE = """entailment\("entailed"\) (\d*\.\d*)""".r

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[FolExpression, Seq[String]],
    evidence: List[FolExpression],
    assumptions: List[WeightedFolEx],
    goal: FolExpression): Seq[Double] = {

    declarations.foreach { dec =>
      dec match {
        case (FolAtom(Variable(pred), args @ _*), argTypes) =>
          for (a <- argTypes)
            require(constants.contains(a), "No contants were found for type '%s' of declared predicate '%s'.".format(a, pred))
        case d => throw new RuntimeException("Only atoms may be declared.  '%s' is not an atom.".format(d))
      }
    }

    val variables: Set[Variable] = findVars(goal);
    var queryParam : String = """entailment\("entailed",""";
    var typeParam : List[String]= List("entail");
    for (v <- variables )
    {
    	  println(v+"\n")
    	  queryParam += """""""+v.name.toUpperCase() + """",""";
     	  entailmentConsequent = FolApplicationExpression(entailmentConsequent, FolVariableExpression(Variable(v.name)));
    	  if (v.name.charAt(0) == 'x')
    	    typeParam ::= "indv";
    	  else if (v.name.charAt(0) == 'e')
    	    typeParam ::= "evnt";
    	  else if (v.name.charAt(0) == 'p')
    	    typeParam ::= "prop";
    	  else throw new RuntimeException ("unsupported type");
    }
    queryParam = queryParam.substring(0, queryParam.length()-1)+"""\) (\d*\.\d*)""";
    ResultsRE = queryParam.r;
    typeParam = typeParam.reverse;
    entailedDec = FolVariableExpression(Variable("entailment")) -> typeParam;
      
    
    val declarationNames =
      (declarations + entailedDec).mapKeys {
        case FolAtom(Variable(pred), _*) => pred
        case FolVariableExpression(Variable(pred)) => pred
      }
    
    
    val mlnFile = makeMlnFile(
      constants + entailedConst,
      declarationNames,
      assumptions,
      goal)
      
    val evidenceFile = makeEvidenceFile(evidence)
    val resultFile = FileUtils.mktemp(suffix = ".res")

    val args = List("-ow", declarationNames.keys.mkString(","), "-q", "entailment")

    /*callAlchemy(mlnFile, evidenceFile, resultFile, args) map {
      case ResultsRE(score) => score.toDouble
      case err => sys.error(err)
    }*/
    
    val result = callAlchemy(mlnFile, evidenceFile, resultFile, args);
    result match 
    {
      case Some(x) =>
      {
        val outputLines =  x.split("\n");
        var maxScore: Double = -1;
        var bestWorld: String = "";
        for (line <- outputLines)
        {
         val lineSplits = line.split("\t");
          if (lineSplits.length == 2)
          {
	          val score = lineSplits(0).toDouble;
	          if (score > maxScore)
	          {
	        	  maxScore = score;
	        	  bestWorld = lineSplits(1);
	          }
	          
          }
        }
        if (maxScore == -1)
        {
            //throw new RuntimeException("no valid output in the result file");
            maxScore = 0;
            bestWorld = "all worlds have equal prob"
            println("" + maxScore + "-->" + bestWorld);
        }          
        else
        {
            println("" + maxScore + "-->" + bestWorld);
        }
        return Seq(maxScore);
        /*var mtch = ResultsRE.findFirstIn(x);
        println(mtch);
        mtch map
        {
          case ResultsRE(score) => {
        	  val infrScore = score.toDouble;
        	  return Some(infrScore);
          }
          case _ => throw new RuntimeException("target line was not found");
        }
        */
      }      
      case None => throw new RuntimeException("empty result file");
    }
    
  }

  private def makeMlnFile(
    constants: Map[String, Set[String]],
    declarationNames: Map[String, Seq[String]],
    assumptions: List[WeightedFolEx],
    goal: FolExpression) = {
    
    /*var reducedConstants = constants;
    for (s <- constants)
    {
      if (s._2.size > 1)
        reducedConstants = reducedConstants + (s._1 -> (s._2-s._2.head )); 
    }
    */
    val tempFile = FileUtils.mktemp(suffix = ".mln")
    FileUtils.writeUsing(tempFile) { f =>
      
      /*reducedConstants.foreach {
        case (name, tokens) => f.write("%s = {%s}\n".format(name, tokens.map(quote).mkString(",")))
      }
      f.write("\n")
      */

      declarationNames.foreach {
        case (pred, varTypes) => f.write("%s(%s)\n".format(pred.replace('-', '_'), varTypes.mkString(",")))
      }
      f.write("\n")

      declarationNames.foreach {
        case ("entailment", varTypes) => f.write("%s %s(%s)\n".format(prior, "entailment", varTypes.indices.map("z" + _).mkString(",")))
        case (pred, varTypes) => f.write("%s %s(%s)\n".format(prior-1, pred.replace('-', '_'), varTypes.indices.map("z" + _).mkString(",")))
      }
      f.write("\n")

      assumptions
        .flatMap {
          case e @ SoftWeightedExpression(folEx, weight) =>
            weight match {
              case Double.PositiveInfinity => Some(HardWeightedExpression(folEx))
              case Double.NegativeInfinity => Some(HardWeightedExpression(-folEx))
              case _ if weight < 0.00001 => None // TODO: Set this threshold
              case _ => Some(e)
            }
          case e @ HardWeightedExpression(folEx, w) => Some(e)
        }
        .foreach {
          case SoftWeightedExpression(folEx, weight) =>
            // TODO: Convert [0,1] weight into alchemy weight
            //            val usedWeight = log(weight / (1 - weight)) / log(logBase) // treat 'weight' as a prob and find the log-odds
            //            f.write(usedWeight + " " + convert(folEx) + "\n")
            val usedWeight = 9 * weight // 5 * (pow(weight, 10)) //TODO: Set these parameters!!
            // TODO: we want to design a function `f` such that, for the simplest examples (only one weighted clause), mln(f(s)) == s
            //   meaning that the probability of entailment (`mln`) using a weight `f(s)` based on similarity score `s <- [0,1]` will be
            //   roughly equal to the similarity score itself.
            f.write("%.15f %s\n".format(usedWeight, convert(folEx)))
          case HardWeightedExpression(folEx, w) =>  if (w == Double.PositiveInfinity)
        	  											f.write(convert(folEx) + ".\n")
        	  										else
        	  										  f.write("%.15f %s\n".format(w, convert(folEx)))
        }

      f.write("\n")
      //println(goal);
      //println(goal -> entailmentConsequent);
      //val un = universalifyGoalFormula(goal -> entailmentConsequent);
      //println(un);
      //println(convert(un));
      //println(average(goal));
      
      val n = coundAnd(goal);
      
      entWeight = 5.7803* pow(n, -1.0043);

      f.write(average(goal));
      f.write("// " + convert(universalifyGoalFormula(goal -> entailmentConsequent)) + ". //(ditAnd)\n")
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

  private def makeEvidenceFile(evidence: List[FolExpression]) = {
    val tempFile = FileUtils.mktemp(suffix = ".db")
    FileUtils.writeUsing(tempFile) { f =>
      evidence.foreach {
        case e @ FolAtom(pred, args @ _*) => f.write(convert(e) + "\n")
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

    val tuffyCalling = "-jar " + System.getenv("TUFFYHOME") + "/tuffy.jar"; 
    val tuffyConfFile = "-conf " + System.getenv("TUFFYHOME") + "/tuffy.conf";
    //val allArgs = "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    
    val query = List("-q", "entailment"); 
    val allArgs = "-marginal" :: "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: query;
    val (exitcode, stdout, stderr) = callAllReturns(None, tuffyCalling :: tuffyConfFile :: allArgs, LOG.isDebugEnabled)
    
    if (exitcode == 0)
    {
    	val results = readLines(result).mkString("\n").trim
    	LOG.debug("results file:\n" + results)
    	return Some(results);
    }
    else
    {
      LOG.debug("Failed with exitcode=%s.\n%s\n%s".format(exitcode, stdout, stderr));
      return Some("");
     }
      
    

    /*exitcode match {
      case 0 => Some(results)
      case _ => throw new RuntimeException("Failed with exitcode=%s.\n%s\n%s".format(exitcode, stdout, stderr))
    }*/
  }
  
  private def findVars(input: FolExpression, bound: Set[Variable] = Set()): Set[Variable] =
	input match {
	  case FolExistsExpression(variable, term) => findVars(term, bound+variable)
	  case _ => bound
  }
  
  private def coundAnd(input: FolExpression): Int =
	input match {
	  case FolExistsExpression(variable, term) => coundAnd(term)
	  case _ => _coundAnd(input)
  }  
  private def _coundAnd(input: FolExpression ): Int =
	input match {
	  case FolAndExpression(first, second) => _coundAnd(first) +  _coundAnd(second) 
      case _ => 1
  }

  
  
  private def average(input: FolExpression, bound: Set[Variable] = Set()): String =
    input match {
      case FolExistsExpression(variable, term) => average(term, bound + variable) // don't add outermost 'exist'
      case _ => _average(input, bound)
    }

  private def _average(input: FolExpression, bound: Set[Variable]): String =
    input match {
      case FolAndExpression(first, second) => _average(first, bound) +  _average(second, bound) 
      case _ => entWeight + "  " + _convert(input -> entailmentConsequent, bound) + "\n"
      /*case FolExistsExpression(variable, term) => "exist " + variable.name + " (" + _convert(term, bound + variable) + ")"
      case FolAllExpression(variable, term) => "forall " + variable.name + " (" + _convert(term, bound + variable) + ")"
      case FolNegatedExpression(term) => "!(" + _convert(term, bound) + ")"
      case FolAndExpression(first, second) => "(" + _convert(first, bound) + " ^ " + _convert(second, bound) + ")"
      case FolOrExpression(first, second) => "(" + _convert(first, bound) + " v " + _convert(second, bound) + ")"
      case FolIfExpression(first, second) => "(" + _convert(first, bound) + " => " + _convert(second, bound) + ")"
      case FolIffExpression(first, second) => "(" + _convert(first, bound) + " <=> " + _convert(second, bound) + ")"
      case FolEqualityExpression(first, second) => "(" + _convert(first, bound) + " = " + _convert(second, bound) + ")"
      case FolAtom(pred, args @ _*) => pred.name.replace("'", "") + "(" + args.map(v => if (bound(v)) v.name else quote(v.name)).mkString(",") + ")"
      case FolVariableExpression(v) => if (bound(v)) v.name else quote(v.name)
      */
    }

  private def convert(input: FolExpression, bound: Set[Variable] = Set()): String =
    input match {
      case FolAllExpression(variable, term) => convert(term, bound + variable) // don't add outermost 'forall'
      case _ => _convert(input, bound)
    }

  private def _convert(input: FolExpression, bound: Set[Variable]): String =
    input match {
      case FolExistsExpression(variable, term) => "exist " + variable.name + "  " + _convert(term, bound + variable) + " "
      case FolAllExpression(variable, term) => "forall " + variable.name + "  " + _convert(term, bound + variable) + " "
      case FolNegatedExpression(term) => "! " + _convert(term, bound) + " "
      case FolAndExpression(first, second) => " " + _convert(first, bound) + " , " + _convert(second, bound) + " "
      case FolOrExpression(first, second) => " " + _convert(first, bound) + " v " + _convert(second, bound) + " "
      case FolIfExpression(first, second) => " " + _convert(first, bound) + " => " + _convert(second, bound) + " "
      case FolIffExpression(first, second) => " " + _convert(first, bound) + " <=> " + _convert(second, bound) + " "
      case FolEqualityExpression(first, second) => " " + _convert(first, bound) + " = " + _convert(second, bound) + " "
      case FolAtom(pred, args @ _*) => pred.name.replace("'", "").replace("-","_") + "(" + args.map(v => if (bound(v)) v.name else quote(v.name)).mkString(",") + ")"
      case FolVariableExpression(v) => if (bound(v)) v.name else quote(v.name)
      
      
      /*
      case FolExistsExpression(variable, term) => "exist " + variable.name + " (" + _convert(term, bound + variable) + ")"
      case FolAllExpression(variable, term) => "forall " + variable.name + " (" + _convert(term, bound + variable) + ")"
      case FolNegatedExpression(term) => "!(" + _convert(term, bound) + ")"
      case FolAndExpression(first, second) => "(" + _convert(first, bound) + " ^ " + _convert(second, bound) + ")"
      case FolOrExpression(first, second) => "(" + _convert(first, bound) + " v " + _convert(second, bound) + ")"
      case FolIfExpression(first, second) => "(" + _convert(first, bound) + " => " + _convert(second, bound) + ")"
      case FolIffExpression(first, second) => "(" + _convert(first, bound) + " <=> " + _convert(second, bound) + ")"
      case FolEqualityExpression(first, second) => "(" + _convert(first, bound) + " = " + _convert(second, bound) + ")"
      case FolAtom(pred, args @ _*) => pred.name.replace("'", "") + "(" + args.map(v => if (bound(v)) v.name else quote(v.name)).mkString(",") + ")"
      case FolVariableExpression(v) => if (bound(v)) v.name else quote(v.name)
      */
    }

  private def quote(s: String) = '"' + s + '"'

}

object TuffyTheoremProver {

  //def findBinary(binDir: Option[String] = None, envar: Option[String] = Some("ALCHEMYHOME"), verbose: Boolean = false) =
    //new AlchemyTheoremProver(FileUtils.findBinary("infer", binDir, envar, verbose))
  
  def findBinary(binDir: Option[String] = None, envar: Option[String] = None, verbose: Boolean = false) =
    new TuffyTheoremProver(FileUtils.findBinary("java", binDir, envar, verbose))  

  def main(args: Array[String]) {
    val parse = new FolLogicParser().parse(_)

    val atp = new TuffyTheoremProver(pathjoin(System.getenv("HOME"), "bin/alchemy/bin/infer"))

    val constants = Map("ind" -> Set("Socrates"))
    val declarations = Map[FolExpression, Seq[String]](FolAtom(Variable("man")) -> Seq("ind"), FolAtom(Variable("mortal")) -> Seq("ind"))
    val evidence = List("man(Socrates)").map(parse)
    val assumptions = List(HardWeightedExpression(parse("all x.(man(x) -> mortal(x))")))
    val goal = parse("mortal(Socrates)")
    println(atp.prove(constants, declarations, evidence, assumptions, goal))

  }
}
