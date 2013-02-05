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

class AlchemyTheoremProver(
  override val binary: String,
  prior: Double = -3,
  var entWeight: Double = 1,
  logBase: Double = E)
  extends SubprocessCallable(binary)
  with ProbabilisticTheoremProver[FolExpression] {

  type WeightedFolEx = WeightedExpression[FolExpression]

  private val LOG = LogFactory.getLog(classOf[AlchemyTheoremProver])

  private val entailedConst = ("entail" -> Set("entailed"))
  private var entailedDec = FolVariableExpression(Variable("entailment")) -> Seq("entail")
  private var entailmentConsequent = FolAtom(Variable("entailment"), Variable("entailed")); //FolVariableExpression(Variable("entailment")) -> Seq("");//= 
  private var ResultsRE = """entailment\("entailed"\) (\d*\.\d*)""".r

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
    
    
/*  // This block to add all variables to the entailment clause. I may need it back one day
    val variables: Set[Variable] = findVars(goal);
    var queryParam : String = """entailment\("entailed",""";
    var typeParam : List[String]= List("entail");
    for (v <- variables )
    {
    	  //println(v+"\n")
    	  queryParam += """""""+v.name.toUpperCase() + """",""";
     	  entailmentConsequent = FolApplicationExpression(entailmentConsequent, FolVariableExpression(Variable(v.name)));
    	  if (v.name.charAt(1) == 'x')
    	    typeParam ::= "indv";
    	  else if (v.name.charAt(1) == 'e')
    	    typeParam ::= "evnt";
    	  else if (v.name.charAt(1) == 'p')
    	    typeParam ::= "prop";
    	  else throw new RuntimeException ("unsupported type");
    }
    queryParam = queryParam.substring(0, queryParam.length()-1)+"""\) (\d*\.\d*)""";
    ResultsRE = queryParam.r;
    typeParam = typeParam.reverse;
    entailedDec = FolVariableExpression(Variable("entailment")) -> typeParam;
 */
    
    val declarationNames =
      (declarations + entailedDec).mapKeys {
        case FolAtom(Variable(pred), _*) => pred
        case FolVariableExpression(Variable(pred)) => pred
      }
    
    
    val mlnFile = makeMlnFile(
      constants + entailedConst,
      declarationNames,
      assumptions,
      evidence,
      goal)
      
    val evidenceFile = makeEvidenceFile(evidence)
    val resultFile = FileUtils.mktemp(suffix = ".res")

    //Adding all predicates to the query force them to be open world
    //This could be slower but, this is the only way to set the predicates to open-world in 
    //Alchamy 2.0 because they removed this option
    //val args = List("-ow", declarationNames.keys.mkString(","), "-q", "entailment")
    //val args = List( "-q", declarationNames.keys.mkString(","))
    
    //all evd are in the mln file
    val args = List( "-q", "entailment")

    //old call for alchemy.
    /*callAlchemy(mlnFile, evidenceFile, resultFile, args) map {
      case ResultsRE(score) => score.toDouble
      case err => sys.error(err)
    }*/
    
    //Output has many lines
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
         val lineSplits = line.split(" ");
          if (lineSplits.length == 2)
          {
	          val score = lineSplits(1).toDouble;
	          if (score > maxScore && lineSplits(0).startsWith("entailment"))
	          {
	        	  maxScore = score;
	        	  bestWorld = lineSplits(0);
	          }
          }
        }
        if (maxScore == -1)
          throw new RuntimeException("no valid output in the result file");
        else
        	return Some(maxScore);
      }      
      case None => throw new RuntimeException("empty result file");
    }
    
  }

  private def makeMlnFile(
    constants: Map[String, Set[String]],
    declarationNames: Map[String, Seq[String]],
    assumptions: List[WeightedFolEx],
    evidence: List[FolExpression],
    goal: FolExpression) = {
    
    var reducedConstants = constants;
    for (s <- constants)
    {
      if (s._2.size > 1)
		{
			var constsList: Set[String] =Set();
			for(c<- s._2)
			{
				if(!c.startsWith("default"))
					constsList = constsList + c;
			}
         reducedConstants = reducedConstants + (s._1 -> constsList); 
		}
    }
    
    val tempFile = FileUtils.mktemp(suffix = ".mln")
    FileUtils.writeUsing(tempFile) { f =>
      reducedConstants.foreach {
        case (name, tokens) => f.write("%s = {%s}\n".format(name, tokens.map(quote).mkString(",")))
      }
      f.write("\n")

      declarationNames.foreach {
        case (pred, varTypes) => f.write("%s(%s)\n".format(pred, varTypes.mkString(",")))
      }
      f.write("\n")

      declarationNames.foreach {
        //different priors for ent and other predicates 
        case ("entailment", varTypes) => f.write("%s !%s(%s)\n".format(-prior, "entailment", varTypes.indices.map("z" + _).mkString(",")))
        case (pred, varTypes) => f.write("%s !%s(%s)\n".format(-prior, pred, varTypes.indices.map("z" + _).mkString(",")))
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
          case e @ HardWeightedExpression(folEx) => Some(e)
        }
        .foreach {
          case SoftWeightedExpression(folEx, weight) =>
            // DONE: Convert [0,1] weight into alchemy weight
            //            val usedWeight = log(weight / (1 - weight)) / log(logBase) // treat 'weight' as a prob and find the log-odds
            //            f.write(usedWeight + " " + convert(folEx) + "\n")
            var usedWeight = min(weight, 0.999);
            usedWeight = max(weight, 0.001);
            usedWeight = -prior + log(usedWeight) - log(1-usedWeight);
            if (usedWeight  > 0)
            	f.write("%.15f %s\n".format(usedWeight, convert(folEx)))
            //val usedWeight = 10 * weight // 5 * (pow(weight, 10)) //DONE: Set these parameters!!
            // DONE: we want to design a function `f` such that, for the simplest examples (only one weighted clause), mln(f(s)) == s
            //   meaning that the probability of entailment (`mln`) using a weight `f(s)` based on similarity score `s <- [0,1]` will be
            //   roughly equal to the similarity score itself.
            
          case HardWeightedExpression(folEx) => f.write(convert(folEx) + ".\n")
        }

      f.write("\n")
      
      val entWeights = Array(0.0, 
 
	/*Best configuration before the technique of anding relating with non-relation predicates
    0.0000,
    0.0000,
    1.2100,
    1.5800,
    0.9950,
    1.0300,
    1.2000,
    1.7650,
    1.3800,
    1.8900,
    4.3850,
    1.5800,
    1.5300,
    1.0500,
    1.4350,
    0.0000,
    0.0000,
    1.7000,
    0.0000,
    0.8600,
    0.0000,
    0.0000,
    0.0000,
    0.0000,
    0.0000,
    0.0000,
    0.0000,
    0.0000,
    0.0000,
    4.8500
    */
    /*1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,*/
    1.0000
    /*      
    1.0000,
    1.0000,
    1.0000,
    0.9930,
    1.0000,
    0.9454,
    0.9834,
    0.8455,
    1.0000,
    0.9225,
    0.6601,
    2.1020,
    1.6146,
    0.4903,
    1.0000,
    0.9183,
    1.0000,
    0.4377,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.0000,
    1.7986
    */
    );
   
      f.write("//begin combination function\n");

      val ands = getAnds(goal);//get a list of the anded predicates
      var nonRelationsMap : Map[FolExpression, Set[Variable]] = Map();
      var relationsMap : Map[FolExpression, Set[Variable]] = Map();//relation predicates are 2 valued and start with r_. 
      															//e.g: agent, patient, in, ...
      var notEqMap : Map[FolExpression, Set[Variable]] = Map();//expressions of the form !(x1=x2)
      var impMap : Map[FolExpression, Set[Variable]] = Map();//expressions of the form a->(b^c^...)
      
      for(expr <- ands )
      {
        val allVars = findAllVars(expr); 
        expr match
        {
	      case FolApplicationExpression(fun, arg) =>{
		        fun match {
		          case FolApplicationExpression (fun2, arg2) =>
		            fun2 match {
		              case FolVariableExpression(v) =>{
		            	  if (v.name.startsWith("r_"))
		            	    relationsMap = relationsMap + ( expr -> allVars);
		            	  else 
		            	    nonRelationsMap = nonRelationsMap + ( expr -> allVars);
		              }		                
		              case _ => nonRelationsMap = nonRelationsMap + ( expr -> allVars);
		            }		            
		          case _=>  nonRelationsMap = nonRelationsMap + ( expr -> allVars); 
		        }
	        }
	      case FolEqualityExpression(first, second)=>; //delete equality expressions because it is already handeled by renaming variables
	      case FolAllExpression(first, second)=>  impMap  = impMap + ( expr -> allVars);
	      case FolNegatedExpression(term)=> {
	        term match {
	          case FolEqualityExpression(first, second) => notEqMap  = notEqMap + ( expr -> allVars);
	          case _ => nonRelationsMap = nonRelationsMap + ( expr -> allVars); //this case will be changed later
	        }
	      };
	      case _ => nonRelationsMap = nonRelationsMap + ( expr -> allVars);
        }
      }
      
      //number of a->ent rules is not = number of anded predicates
      //val n = coundAnd(goal);
      var n = 0;
      for(nonRelationExpr<- nonRelationsMap )
      {
    	  var printedAtLeastOnce = false;
	      for(relationExpr<- relationsMap )
	      {
	    	  if ((nonRelationExpr._2 & relationExpr._2).size != 0)
	    	  {
	    		  printedAtLeastOnce = true;
	    		  n = n +1 ;
	    	  }
	      }
	      if (!printedAtLeastOnce)
	        n = n +1 ;
      }
      n += notEqMap.size;   //Each notEqual expression corresponds to an extra line in the combination function
      n += impMap.size;   //Each imp expression corresponds to an extra line in the combination function
      						//THis may change 
      
      //if n is not in the list of weights, set it to 1
      if (n >= entWeights.size)
    	  entWeight = 0.4;
      else
    	  entWeight = entWeights(n);
      
      val allGoalVariables: Set[Variable] = findAllVars(goal);      

      //write relation predicates
      for(nonRelationExpr<- nonRelationsMap )
      {
    	  var printedAtLeastOnce = false;
	      for(relationExpr<- relationsMap )
	      {
	    	  if ((nonRelationExpr._2 & relationExpr._2).size != 0)
	    	  {
	    		  f.write(entWeight + "  " + convert((nonRelationExpr._1 & relationExpr._1) -> entailmentConsequent, allGoalVariables) + "\n");
	    		  printedAtLeastOnce = true;
	    	  }
	      }
	      if (!printedAtLeastOnce)
	        f.write(entWeight + "  " + convert(nonRelationExpr._1 -> entailmentConsequent, allGoalVariables) + "\n");
      }
      
      //write notEqual predicates
      for(notEqExpr <- notEqMap )
      {
          var oneLine = notEqExpr._1;
	      for(nonRelationExpr <- nonRelationsMap )
	      {
	    	  if ((notEqExpr._2 & nonRelationExpr._2).size != 0)
	    	  {
	    		  oneLine = oneLine & nonRelationExpr._1;
	    	  }
	      }
	      f.write(entWeight + "  " + convert(oneLine -> entailmentConsequent, allGoalVariables) + "\n");
      }
      
      //write imp expressions 
      for(impExpr <- impMap )
      {
	      f.write(entWeight + "  " + convert(impExpr._1 -> entailmentConsequent, allGoalVariables) + "\n");
      }
      
      
      //f.write(average(goal));  a->ent, where a is one of the anded formulas
      f.write("//end combination function\n");

      //normal anding
      //f.write("// " + convert(universalifyGoalFormula(goal -> entailmentConsequent)) + ". //(ditAnd)\n")
      /*
      f.write("//begin evd part\n");
      evidence.foreach {
        case e @ FolAtom(pred, args @ _*) => f.write(convert(e) + ".\n")
        case e => throw new RuntimeException("Only atoms may be evidence.  '%s' is not an atom.".format(e))
      }   
      f.write("//end evd part\n");
      */
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
      f.write("//\n");
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

    //lifted belife propagation works better
    //val allArgs = "-bp" :: "-lifted" :: "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    //Dunno, but it seems that MC-SAT is better
    //val allArgs = "-ptpe" :: "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    val allArgs = "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    println("Args: " + allArgs);
    //val (exitcode, stdout, stderr) = callAllReturns(None, allArgs, LOG.isDebugEnabled, false);
    val (exitcode, stdout, stderr) = callAllReturns(None, allArgs, true);

    val results = readLines(result).mkString("\n").trim

    LOG.debug("results file:\n" + results)

    exitcode match {
      case 0 => Some(results)
      case _ => throw new RuntimeException("Failed with exitcode=%s.\n%s\n%s".format(exitcode, stdout, stderr))
    }
  }
  
  //find outer most variables in the exist clauses
  private def findVars(input: FolExpression, bound: Set[Variable] = Set()): Set[Variable] =
	input match {
	  case FolExistsExpression(variable, term) => findVars(term, bound+variable)
	  case _ => bound
  }
  
  //find all variables used in the clause
  private def findAllVars(input: FolExpression): Set[Variable] =
  {
	input match {
      case FolExistsExpression(variable, term) => findAllVars(term) + variable;
      case FolAllExpression(variable, term) => findAllVars(term) + variable;
      case FolNegatedExpression(term) => findAllVars(term);
      case FolAndExpression(first, second) => findAllVars(first) ++ findAllVars(second);
      case FolOrExpression(first, second) => findAllVars(first) ++ findAllVars(second);
      case FolIfExpression(first, second) => findAllVars(first) ++ findAllVars(second);
      case FolIffExpression(first, second) => findAllVars(first) ++ findAllVars(second);
      case FolEqualityExpression(first, second) => findAllVars(first) ++ findAllVars(second);
      case FolApplicationExpression(fun, arg) =>{
        fun match {
          case FolVariableExpression (v) => findAllVars(arg);
          case _=> findAllVars(arg) ++ findAllVars(fun);
        }
      }
      //case FolAtom(pred, args) =>  
      case FolVariableExpression(v) => Set(v);
      case _ => {
    	  println(input.toString());
    	  Set();
      } 
    }
  }
  
  //count the anded predicates
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

  //get a list of the anded predicates
  private def getAnds(input: FolExpression): List[FolExpression] =
    input match {
      case FolExistsExpression(variable, term) => getAnds(term) // don't add outermost 'exist'
      case _ => _getAnds(input)
    }

  private def _getAnds(input: FolExpression): List[FolExpression] =
    input match {
      case FolAndExpression(first, second) => _getAnds(first) ++  _getAnds(second) 
      case _ => List(input);
    }
  
  
  //average instead of anding 
  private def average(input: FolExpression, bound: Set[Variable] = Set()): String =
    input match {
      case FolExistsExpression(variable, term) => average(term, bound + variable) // don't add outermost 'exist'
      case _ => _average(input, bound)
    }

  private def _average(input: FolExpression, bound: Set[Variable]): String =
    input match {
      case FolAndExpression(first, second) => _average(first, bound) +  _average(second, bound)
      //case _ => entWeight + "  " + _convert(input -> entailmentConsequent, bound) + "\n"
      case _ => entWeight + "  " + convert(universalifyGoalFormula(input -> entailmentConsequent), bound) + "\n"
      
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

  //convert a FOLExpression to an alchamy string 
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
      case FolEqualityExpression(first, second) => "(" + _convert(first, bound) + " = " + _convert(second, bound) + ")"
      case FolAtom(pred, args @ _*) => pred.name.replace("'", "") + "(" + args.map(v => if (bound(v)) v.name.toLowerCase() else quote(v.name)).mkString(",") + ")"
      case FolVariableExpression(v) => if (bound(v)) v.name.toLowerCase() else quote(v.name)
    }

  private def quote(s: String) = '"' + s + '"'

}

object AlchemyTheoremProver {

  def findBinary(binDir: Option[String] = None, envar: Option[String] = Some("ALCHEMYHOME"), verbose: Boolean = false) =
    //new AlchemyTheoremProver(FileUtils.findBinary("liftedinfer", binDir, envar, verbose))
    new AlchemyTheoremProver(FileUtils.findBinary("infer", binDir, envar, verbose))

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
