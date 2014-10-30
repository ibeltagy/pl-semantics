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
import java.io.File
import edu.umd.cs.psl.evaluation.resultui.printer.ListAtomPrintStream
import util.PSLInterface

class PSLTheoremProver(
  )
  
  extends ProbabilisticTheoremProver[FolExpression] {

  /*if (PSLTheoremProver.cp == "")
  {
    var out = new StringBuilder
	Process("cat", Seq("psl/cp.txt")) ! (ProcessLogger(out.append(_), System.err.println(_)))
	PSLTheoremProver.cp = out.toString()
  }*/
  
  val prior: Double = -3;
  var entWeight: Double = 1;
  
  type WeightedFolEx = WeightedExpression[FolExpression]

  private val LOG = LogFactory.getLog(classOf[PSLTheoremProver])


  private var entailmentConsequent_h:FolExpression = FolVariableExpression(Variable("entailment_h")); 
  private var entailmentConsequent_t:FolExpression = FolVariableExpression(Variable("entailment_t"));
  private val entailedConst = ("ent" -> Set("ent_h", "ent_t"))
  private var entailmentConsequent:FolExpression = FolAtom(Variable("entailment"), Variable(""));
    
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
    
    // This block adds all variables to the entailment clause. 
    //val variables: Set[Variable] = findAllVars(goal);
    var typeParam_h : List[String]= List();
    var typeParam_t : List[String]= List();
    entailmentConsequent_t = FolAtom(Variable("entailment_t"), Variable("")); 
	entailmentConsequent_h = FolAtom(Variable("entailment_h"), Variable(""));

    val entailedDec_h = FolVariableExpression(Variable("entailment_h")) -> typeParam_h;
    val entailedDec_t = FolVariableExpression(Variable("entailment_t")) -> typeParam_t;
    val entailedDec = FolVariableExpression(Variable("entailment")) -> List();
 
    
    val declarationNames =
      (declarations + entailedDec_h + entailedDec_t + entailedDec).mapKeys {
        case FolAtom(Variable(pred), _*) => pred
        case FolVariableExpression(Variable(pred)) => pred
      }
    
    
    val mlnFile = makeMlnFile(
      constants + entailedConst,
      declarationNames,
      assumptions,
      evidence,
      goal)
      
    //val evidenceFile = makeEvidenceFile(evidence)
    //val resultFile = FileUtils.mktemp(suffix = ".res")

    //val args = List( "-q", "entailment_h,entailment_t")

    try 
    {

    	//113,244,339,345,421,436  (zeros)
    	//347,352,406,498,509    (memory)
    	//498,517,664,960,1431   (memroy)
    	//InnerJoin, OuterJoin, OuterJoinWithDummy;
    	//var resultScore = callPSL(mlnFile, "InnerJoin", timeout);
    	var resultScore = callPSL(mlnFile, "OuterJoin");
    	
    	//if (resultScore == 0 )
    	//	resultScore = callPSL(mlnFile, "OuterJoin", timeout);
    	//if (resultScore == 0 )
    	// resultScore = callPSL(mlnFile, "OuterJoinWithDummy", timeout);    	
    	  
    	return resultScore;
    	//Process("mvn", Seq("compile", "-f", "psl/pom.xml")) ! (ProcessLogger(System.err.println(_), System.err.println(_)))
    	//Process("mvn", Seq("exec:java", "-Dexec.mainClass=psl.TextInterface", "-f", "psl/pom.xml")) ! (ProcessLogger(System.err.println(_), System.err.println(_)))
    	//println (PSLTheoremProver.cp);
    	//callAlchemy(mlnFile, evidenceFile, resultFile, args) match {
	    //  case Some(t) => Some(t.toDouble)
	    //  case _ => throw new RuntimeException("no valid output in the result file");
    	//}
    }catch 
    {
    	case e: Exception =>{
    	  System.err.println (e);
    	  return Seq(-2.0, -2.0);
    	}   				 
    }
    
  }
  
  //mode: InnerJoin, OuterJoin, OuterJoinWithDummy
  private def callPSL (mlnFile: String, mode: String):Seq[Double] = { 
    var entailmentLine = "";
	var entailmentHLine = "";
    var entailmentTLine = "";
    val timeoutVal = Sts.opts.timeout match {
      case Some(time) => time.toString();
      case _ =>"0";
    };

    val l:ListAtomPrintStream = new ListAtomPrintStream();
    PSLInterface.call(Array(mlnFile, mode, timeoutVal, Sts.opts.groundLimit.toString, Sts.opts.metaW.toString, Sts.opts.relW.toString));
    //TextInterface.main(mlnFile, mode, timeoutVal, Sts.opts.groundLimit.toString, Sts.opts.metaW.toString, Sts.opts.relW.toString);
    val values = l.getValues(); //(entailment_h, entailment_t, entailment)
    LOG.trace(values);
    
    val entHscore:Double = if(values.containsKey("entailment_h()"))
    	  				values.get("entailment_h()")
    	  			  else 0.0;
    val entTscore:Double = if(values.containsKey("entailment_t()"))
    	  				values.get("entailment_t()")
    	  			  else 0.0;
    
    if(Sts.opts.task == "rte")
	{
    	return Seq(entHscore);
	}
	else
	{
		return Seq(entHscore, entTscore)
	}  
    
    /*
    LOG.trace("Calling: ant -buildfile psl/build.xml run -Darg0=%s -Darg1=%s -Darg2=%s -Darg3=%s -Darg4=%s -Darg5=%s".format(mlnFile, mode, 
    											timeoutVal, Sts.opts.groundLimit, Sts.opts.metaW, Sts.opts.relW));
    val proc = Process("ant", Seq("-buildfile", "psl/build.xml", "run", "-Darg0="+mlnFile, "-Darg1="+mode, "-Darg2="+timeoutVal, 
    										"-Darg3="+Sts.opts.groundLimit, "-Darg4="+Sts.opts.metaW, "-Darg5="+Sts.opts.relW)).run(
    ProcessLogger(l=>{
          System.out.println(l)
          if (l.startsWith("     [java] entailment() V="))
        	  	entailmentLine = l;
			 else if (l.startsWith("     [java] entailment_h() V="))
            entailmentHLine = l;
			 else if (l.startsWith("     [java] entailment_t() V="))
            entailmentTLine = l;

      }, System.err.println(_)))
    
    var exitcode = 1;
	Sts.opts.timeout match {
	  case Some(time) => {
	    val t = new Thread { override def run() { exitcode = proc.exitValue() } }
	    t.start()
	    t.join(time)
	    proc.destroy();
	  }
	  case _ => exitcode = proc.exitValue()
	}

    println ("exitcode = " + exitcode)
    if (exitcode != 0){
    	println("ERROR: PSL inference fails.")
    	if(Sts.opts.task == "rte")
    	  return Seq(-1);
    	else return Seq(-1, -1);
    }



	def lineToScore (line: String) = 
	{
        if (line == "")
              0;
        else
        {
              val lineSplits = line.split("\\[");
              val scoreWithBracket = lineSplits(lineSplits.length-1);
              scoreWithBracket.substring(0, scoreWithBracket.length()-1).toDouble;
        }
	}
	

	 if(Sts.opts.task == "rte")
	 {
			return Seq(lineToScore(entailmentHLine));

		/*		if (entailmentLine == "")
				  return 0;
				else
				{
				  val lineSplits = entailmentLine.split("\\[");
				  val scoreWithBracket = lineSplits(lineSplits.length-1);
				  return scoreWithBracket.substring(0, scoreWithBracket.length()-1).toDouble;
				  //return entailmentLine.substring(16, entailmentLine.length()-1).toDouble;      
				}
		*/
	 }
	 else
	 {
		var entHscore = lineToScore(entailmentHLine);
		var entTscore = lineToScore(entailmentTLine);
	  return Seq(entHscore, entTscore)
	 }
    */
    
  }

  private def makeMlnFile(
    constants: Map[String, Set[String]],
    declarationNames: Map[String, Seq[String]],
    assumptions: List[WeightedFolEx],
    evidence: List[FolExpression],
    goal: FolExpression) = {
    
    //val pslFilePath = "psl/run/%s.psl".format(Sts.pairIndex);
    //val tempFile = FileUtils.mktemp(suffix = ".mln") 
    //val pslFilePath = System.getProperty("user.dir")+"/psl/run/%s.psl".format(Sts.pairIndex);
    val pslFilePath = FileUtils.mktemp(suffix = ".psl") 
    val pslFile = new java.io.PrintWriter(new File(pslFilePath))
    try { 

        				
      //=================Headers      
    	/*pslFile.write(
			"package psl;\n" +
			"import edu.umd.cs.psl.groovy.*;\n" +
			"import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;\n" +
			"import edu.umd.cs.psl.model.DistanceNorm;\n" +
			"import edu.umd.cs.psl.model.function.AttributeSimilarityFunction;\n" +
			"import edu.umd.cs.psl.config.*;\n" +
			"import edu.umd.cs.psl.model.predicate.type.*;\n" +
			"import edu.umd.cs.psl.ui.functions.textsimilarity.*;\n" +
			"println \"Hellooooooooooooo\";\n" );
		*/

    	//=================Similarity function
        /*pslFile.writeLine(
		"class Sim%s implements AttributeSimilarityFunction {\n".format(PSLTheoremProver.pairIndx) +
				"private HashMap sim;\n" +
				"@Override\n" +
				"public double similarity(String a, String b) {\n" +
					"Double score = sim.get(a);\n" +
					"if (score == null)\n" +
						"throw new Exception(\"score for \" + a + \" not found\");\n" +
					"else return score.value; }	\n" +
				"Sim%s (){\n".format(PSLTheoremProver.pairIndx) +
        			"sim = new HashMap<String, Double>();\n" +
        			"BufferedReader fr =  new BufferedReader(new FileReader(\"sim/%s.txt\"));\n".format(PSLTheoremProver.pairIndx) +
        			"String l;\n" +
        			"while((l = fr.readLine()) != null){\n" +
        				"String[] splits = l.split(\",\");\n" +
        				"sim.put(splits[0], splits[1].toDouble());\n" +
        				"}}}")
        */

        //=================begining of rules
        //pslFile.write(
		//	"PSLModel m = new PSLModel(this);\n" +
		//	"m.add function: \"sim\" , name1: Text, name2: Text, implementation: new Sim%s();\n".format(PSLTheoremProver.pairIndx) +
		//	"m.add predicate: \"all\", arg1: Entity;\n");

       //=================Predicate declarations
      	pslFile.write("predicate,all_h,1\n")
      	pslFile.write("predicate,all_t,1\n")
      	pslFile.write("predicate,all,1\n")
      	pslFile.write("predicate,dummyPred,1\n")
      	pslFile.write("predicate,negationPred_n_dh,1\n")
      	pslFile.write("predicate,negationPred_n_dt,1\n")
      	declarationNames.foreach {
      	    case ("entailment_h", varTypes) => pslFile.write("predicate,%s,%s\n".format("entailment_h", 0))
      	    case ("entailment_t", varTypes) => pslFile.write("predicate,%s,%s\n".format("entailment_t", 0))
			case (pred, varTypes) => {
				//pslFile.write("m.add predicate: \"%s\", %s open: true;\n".format(pred, varTypes.indices.map("arg" + _+": Entity, ").mkString(""))) 
			  pslFile.write("predicate,%s,%s\n".format(pred, varTypes.length))
			  if((pred.contains("agent")||pred.contains("patient"))&& Sts.opts.funcConst)
				  pslFile.write("constraint,%s\n".format(pred))
			}
    	}
       //=================Priors
		declarationNames.foreach {
			 //different priors for ent and other predicates
		    //m.add Prior.Simple, on : ent, weight: 0.01

		  //case ("entailment_h", varTypes) => pslFile.write("m.add Prior.Simple, on: %s, weight: 0.01;\n".format("entailment_h"))
		  //case ("entailment_t", varTypes) => pslFile.write("m.add Prior.Simple, on: %s, weight: 0.01;\n".format("entailment_t"))
		  //case ("entailment", varTypes) => pslFile.write("m.add Prior.Simple, on: %s, weight: 0.01;\n".format("entailment"))
		  //case (pred, varTypes) => pslFile.write("m.add Prior.Simple, on: %s, weight: 0.1;\n".format(pred))
		    case ("entailment_h", varTypes) => pslFile.write("prior,%s,0.01\n".format("entailment_h"))
			case ("entailment_t", varTypes) => pslFile.write("prior,%s,0.01\n".format("entailment_t"))
			case ("entailment", varTypes) => pslFile.write("prior,%s,0.01\n".format("entailment"))
			case (pred, varTypes) => pslFile.write("prior,%s,0.1\n".format(pred))
		}
		
       //=================Inference rules		
		//var similarityTable: List[(String, Double)] = List(); 
		//var lastSimilarityID:Integer = 0;
		assumptions
        .flatMap {
          case e @ SoftWeightedExpression(folEx, weight) =>
            weight match {
              //case Double.PositiveInfinity => Some(HardWeightedExpression(folEx))
              case Double.NegativeInfinity => None ;//Some(HardWeightedExpression(-folEx))
              case _ => Some(e)
            }
          case e @ _ => Some(e)
        }.foreach { e => 
          e match {
          	  case PriorExpression(folExp, weight) => 
	            	None
	            	//f.write("%.5f %s\n".format(weight, convert(folExp)))
	          case GoalExpression(folExp, weight) =>
	          {	        	  	            
	        	  if(weight == Double.PositiveInfinity)
	        	    //pslFile.write("rule,avg,%s\n".format(convert(universalifyGoalFormula(breakVariableBinding(first) -> entailmentConsequent_h)))) //normal anding
	        		  pslFile.write("rule,avg,%s,inf\n".format(convert(folExp))) //normal anding
	        	  else
	        		  pslFile.write("rule,and,entailment_h()&entailment_t()>>entailment(),inf\n");
	        	     
	          }
	          case SoftWeightedExpression(folExp, weight) =>
	            var usedWeight = min(weight, 1);
	            usedWeight = max(usedWeight, 0);
	            if (usedWeight  > 0)
	            {
	              removeOuterUnivs(folExp) match {
	                case FolIfExpression(lhs, rhs) => {
		              val lhsAnds = getAnds(lhs)
		              val lhsVars = findAllVars(lhs) 
		              val lhsString = convert(lhs)
		              val lhsSimString = lhsAnds.flatMap {
		              	case FolAtom(pred, args @ _*) if (args.length == 1)=> List(pred.name)
		              	case _ =>None
		              }.mkString("-")
		              
		              val rhsAnds = getAnds(rhs)
		              val rhsSimString = rhsAnds.flatMap {
		              	case FolAtom(pred, args @ _*) if (args.length == 1)=> List(pred.name)
		              	case _ =>None
		              }.mkString("-")
		              
		              //similarityTable ::= (lhsSimString+"#"+rhsSimString, usedWeight) ;
		              //lastSimilarityID = lastSimilarityID+1;
		              //similarityTable ::= (lastSimilarityID.toString(), usedWeight) ;
		              
		              rhsAnds.foreach(rhsAnd => {
		            	  val rhsVar = findAllVars(rhsAnd)
		            	  val missingVars = (rhsVar &~ lhsVars).toSet;
		            	  var extendedLhsString = lhsString;
		            	  val allString = rhsAnd match {
		            	    case FolAtom(pred, args @ _*) if pred.name.endsWith("_dh") => "all_h" 
		            	    case FolAtom(pred, args @ _*) if pred.name.endsWith("_dt") => "all_t"
		            	    case _ => throw new RuntimeException("unsupported expression: %s".format(rhsAnd));
		            	  }
		            	  
		            	  missingVars.foreach(v => {
		            	     //extendedLhsString = "(%s & all(%s))".format(extendedLhsString, v.name.toUpperCase())
		            	    extendedLhsString = "%s&%s(%s)".format(extendedLhsString, allString, v.name.toUpperCase())
		            	  })
		            	  val rhsString = convert(rhsAnd)
		            	  //pslFile.writeLine("m.add rule: (%s & sim(\"%s\", \"%s\")) >> %s, constraint: true;"
		            	  pslFile.write("rule,and,%s&sim(\"%s\",\"%s\")>>%s,1\n"
		            	      //.format(extendedLhsString, lastSimilarityID.toString(), "", rhsString))
		            	      //.format(extendedLhsString, lhsSimString, rhsSimString, rhsString))
		            	      .format(extendedLhsString, "%.3f".format(usedWeight), "", rhsString))
		              })	                  
	                }
	                case _ => throw new RuntimeException("unsupported infernece rule format"); 
	              }
	            }
	          case HardWeightedExpression(folExp) => throw new RuntimeException("only simple inference rules are accepted. %s is not supported".format(folExp));
         }
       }
       
       var breakVariableBindingCounter = 0;
	   def breakVariableBinding(e: FolExpression): FolExpression = {
	     return e;
	      e match {
	        case FolParseExpression(exps) => FolParseExpression( exps.map(e=> (breakVariableBinding(e._1) , e._2) ) )
	        case FolVariableExpression(v) => {
	          breakVariableBindingCounter = breakVariableBindingCounter + 1;
	          FolVariableExpression(Variable(v.name+"b"+breakVariableBindingCounter)) 
	        }
	        case FolApplicationExpression(fun, arg) => {
	          fun match {
	            case FolVariableExpression(v) => FolApplicationExpression(fun, arg)
	            //case FolApplicationExpression(fun1, arg2) => FolApplicationExpression(FolApplicationExpression(fun1, breakVariableBinding(arg2)), breakVariableBinding(arg)) 
	            case FolApplicationExpression(fun1, arg2) => FolAndExpression(
	                FolApplicationExpression(FolApplicationExpression(fun1, arg2), breakVariableBinding(arg)), 
	                FolApplicationExpression(FolApplicationExpression(fun1, breakVariableBinding(arg2)), arg))
	          }
	        }
	        case _ =>
	          e.visitStructured(breakVariableBinding, e.construct)
	      }
	   }
        
       //=================Goal
/*	    Sts.opts.task match {
      	//case "rte" => pslFile.write("m.add rule: %s, constraint: true;\n".format(convert(universalifyGoalFormula(goal -> entailmentConsequent)))) //normal anding
         case "rte" =>pslFile.write("rule,min,%s\n".format(convert(universalifyGoalFormula(goal -> entailmentConsequent)))) //normal anding
      	//case "sts" => pslFile.write("m.add rule: %s, constraint: true;\n".format(convert(universalifyGoalFormula(goal -> entailmentConsequent)))) //normal anding
         case "sts" => {
           def writeTwoGoals(input: FolExpression):Unit = {
			input match {
		      case FolExistsExpression(variable, term) => writeTwoGoals(term)
		      case FolAndExpression(first, second) => {
		         pslFile.write("rule,avg,%s\n".format(convert(universalifyGoalFormula(breakVariableBinding(first) -> entailmentConsequent_h)))) //normal anding
            	 pslFile.write("rule,avg,%s\n".format(convert(universalifyGoalFormula(breakVariableBinding(second) -> entailmentConsequent_t)))) //normal anding
            	 pslFile.write("rule,and,entailment_h()&entailment_t()>>entailment()\n");
		      }
		      case _ => {
		        pslFile.write("rule,avg,%s\n".format(convert(universalifyGoalFormula(goal -> entailmentConsequent)))) //normal anding
            	 throw new RuntimeException("in STS, goal should be (Sent1)&(Sent2)")
		        }
		      }
           }
         writeTwoGoals(goal);
         }
       }
        */
        

       //=================Similarity File      
        /* val simFile = new java.io.PrintWriter(new File("psl/run/%s.sim".format(Sts.pairIndex)))
         similarityTable.foreach(simEntry =>{
    	   simFile.write("%s,%s\n".format(simEntry._1, simEntry._2))
    	})
    	simFile.close();
    	  */ 
       

       //=================Evidences
       //pslFile.write(
	   //	"DataStore data = new RelationalDataStore(m);\n" +
	   //	"data.setup db : DatabaseDriver.H2;\n");
		 var allConst_h:Set[Int] = Set();
		 var allConst_t:Set[Int] = Set();
	     evidence.foreach {
	        case e @ FolAtom(pred, args @ _*) => 
	          		pslFile.write(
	          		    //"data.getInserter(%s).insert(%s);".format(pred.name, args.map(a => {
	          		    "data,%s,%s\n".format(pred.name, args.map(a => {
	          					val const = a.name.substring(2).toInt+1000*min(a.name.charAt(0).toLower - 103, 2);
	          					if (a.name.charAt(0) == 'h')
	          						allConst_t += const; //yes, add it to allConst_t not allConst_h. This is not a typo 
	          					else if (a.name.charAt(0) == 't' )
	          						allConst_h += const;
	          					else throw new RuntimeException("Unknown constant type %s".format(a.name));
	          					const;
	          			}).mkString(","))
	          			);
	        //case e => throw new RuntimeException("Only atoms may be evidence.  '%s' is not an atom.".format(e))
	         case e => LOG.trace( "Non atomic evidence:  '%s' ".format(e));
	    }
	    pslFile.write("data,dummyPred,999999\n");
	    //Generate evidences for predicate "all"
	     //allConst_h.foreach (const=>pslFile.write("data,all_h,%s\n".format(const)))
	     //allConst_t.foreach (const=>pslFile.write("data,all_t,%s\n".format(const)))
       //=================Query
		pslFile.write(
		    //"ConfigManager cm = ConfigManager.getManager();\n" +
		    //"ConfigBundle exampleBundle = cm.getBundle(\"example\");\n" +
		    //"def result = m.mapInference(data.getDatabase(), exampleBundle);\n" +

		    //"def result = m.mapInference(data.getDatabase());\n" +
		    //"result.printAtoms(entailment_h, false);")
		    //"query,entailment_h")
		    "query.\n")
    
	   pslFile.close();
    }
    pslFilePath;
    	/*


      f.write("\n")
      
      //begin combination function...........................
      
      /*The function below is not used, but it is a good documentation of what should be done
       * 
      
      var ands :List[FolExpression] = List();//get a list of the anded predicates
      var nonRelationsMap : List[(FolExpression, Set[Variable])] = List();
      var relationsMap : List[(FolExpression, Set[Variable])] = List();//relation predicates are 2 valued and start with r_. 
      															//e.g: agent, patient, in, ...
      var notEqMap : List[(FolExpression, Set[Variable])] = List();//expressions of the form !(x1=x2)
      var impMap : List[(FolExpression, Set[Variable])] = List();//expressions of the form a->(b^c^...)
      
      def extractPartsOfExpression (expr: FolExpression) = {
	      ands = getAnds(expr);
	      nonRelationsMap = List();
	      relationsMap = List();
	      notEqMap = List();
	      impMap = List();
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
			            	    relationsMap = (expr, allVars) :: relationsMap  ;
			            	  else
			            	    nonRelationsMap = (expr, allVars) :: nonRelationsMap;
			              }		                
			              case _ => nonRelationsMap = (expr, allVars) :: nonRelationsMap;
			            }
			          case FolVariableExpression(v) =>{
			        	  if (!v.name.startsWith("topic_"))
			        		  nonRelationsMap = (expr, allVars) :: nonRelationsMap;
			          }
			          case _=>  nonRelationsMap = (expr, allVars) :: nonRelationsMap;
			        }
		        }
		      case FolEqualityExpression(first, second)=>; //delete equality expressions because it is already handeled by renaming variables
		      case FolAllExpression(first, second)=>  impMap  = (expr, allVars) :: impMap;
		      case FolNegatedExpression(term)=> {
		        term match {
		          case FolEqualityExpression(first, second) => notEqMap  = (expr, allVars) :: notEqMap;
		          case _ => nonRelationsMap = (expr, allVars) :: nonRelationsMap; //this case will be changed later
		        }
		      };
		      case _ => nonRelationsMap = (expr, allVars) :: nonRelationsMap;
	        }
	      }
      }
      //Chopping levels: type of mini-clauses
      //rp, prp
      val chopLvl  = Sts.opts.get("-chopLvl") match {
			case Some(thr) => thr;
			case _ => "rp";
	  }
	  */
      
    }
    tempFile
    */
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

  /*private def callAlchemy(mln: String, evidence: String, result: String, args: List[String] = List()): Option[String] = {
    if (LOG.isDebugEnabled) {
      LOG.debug("mln file:\n" + readLines(mln).mkString("\n").trim)
      LOG.debug("evidence file:\n" + readLines(evidence).mkString("\n").trim)
    }
    else if (LOG.isInfoEnabled) {
      LOG.info("mln file:\n" + readLines(mln).mkString("\n").trim)
    }
    

    //lifted belife propagation works better
    //val allArgs = "-bp" :: "-lifted" :: "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    //Dunno, but it seems that MC-SAT is better
    //val allArgs = "-ptpe" :: "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    val allArgs = "-i" :: mln :: "-e" :: evidence :: "-r" :: result :: args;
    val timeout = Sts.opts.get("-timeout") match {
			case Some(t) => Some(t.toLong);
			case  _=> None;
		}
    val (exitcode, stdout, stderr) = callAllReturns(None, allArgs, LOG.isDebugEnabled, timeout);
	val out = new StringBuilder
	val err = new StringBuilder
 
    var command = "grep entailment_h "+result+" | awk '{print $2}'| sort -n -r  | head -n 1"
    Process("/bin/sh", Seq("-c", command)) ! (ProcessLogger(out.append(_).append("\n"), System.err.println(_)))
    
    val score1 = out.mkString("").trim().toDouble;
    out.clear();
    
    command = "grep entailment_t "+result+" | awk '{print $2}'| sort -n -r  | head -n 1"
    Process("/bin/sh", Seq("-c", command)) ! (ProcessLogger(out.append(_).append("\n"), System.err.println(_)))
    
    val score2 = out.mkString("").trim().toDouble;
    out.clear();
    
    val score  = task match {
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
  */
  
  private def removeOuterUnivs(input: FolExpression): FolExpression = 
  {
     input match {
	  case FolAllExpression(variable, term) => removeOuterUnivs(term)
	  case _ => input
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
      case FolNegatedExpression(term) => {
       //"!(" + _convert(term, bound) + ")"
	     term match {
	       //TODO: check if it is NonSymmetric or Unequal ??
	   	   case FolEqualityExpression(first, second) => "#Unequal(%s,%s)".format(_convert(first, bound), _convert(second, bound))
	   	   case _ => "dummyPred(Z)"
	   	 } 
      }
         
      //case FolAndExpression(first, second) => "(" + _convert(first, bound) + " & " + _convert(second, bound) + ")"
      case FolAndExpression(first, second) => {
        if (first.isInstanceOf[FolEqualityExpression] )
    	  _convert(second, bound) 
    	else if (second.isInstanceOf[FolEqualityExpression] )
    	  _convert(first, bound)
    	else
    	  _convert(first, bound) + "&" + _convert(second, bound)
      } 
      case FolOrExpression(first, second) => "(" + _convert(first, bound) + " v " + _convert(second, bound) + ")"
      //case FolIfExpression(first, second) => "(" + _convert(first, bound) + " >> " + _convert(second, bound) + ")"
      case FolIfExpression(first, second) =>  _convert(first, bound) + ">>" + _convert(second, bound) 
      case FolIffExpression(first, second) => "(" + _convert(first, bound) + " <=> " + _convert(second, bound) + ")"
      case FolEqualityExpression(first, second) => "dummyPred(Z)"
        	//"(" + _convert(first, bound) + " = " + _convert(second, bound) + ")";	
        	//both variables of the same type
	        /* This part is not needed anymore because we do not have separate types for Events and Indvs anymore. 
	        * if (first.asInstanceOf[FolVariableExpression].variable.name.charAt(1) == 
	        		second.asInstanceOf[FolVariableExpression].variable.name.charAt(1))
	        	"(" + _convert(first, bound) + " = " + _convert(second, bound) + ")";
	        else
	        	// dummy exp
	        	"(" + _convert(first, bound) + " = " + _convert(second, bound) + ")";
	        */
      //case FolAtom(pred, args @ _*) => pred.name.replace("'", "") + "(" + args.map(v => if (bound(v)) v.name.toLowerCase() else quote(v.name)).mkString(",") + ")"
      case FolAtom(pred, args @ _*) => pred.name.replace("'", "") + "(" + args.map(v => v.name.toUpperCase()).mkString(",") + ")"
      case FolVariableExpression(v) => v.name.toUpperCase()//if (bound(v)) v.name.toLowerCase() else quote(v.name)
    }
  



  private def quote(s: String) = '"' + s + '"'

}

object PSLTheoremProver {

  private var cp = "";

  def main(args: Array[String]) {

    Process("mvn", Seq("compile", "-f", "psl/pom.xml")) ! (ProcessLogger(System.err.println(_), System.err.println(_)))
    Process("mvn", Seq("exec:java", "-Dexec.mainClass=psl.Template", "-f", "psl/pom.xml")) ! (ProcessLogger(System.err.println(_), System.err.println(_)))
    
    val pw = new java.io.PrintWriter(new File("psl/src/main/java/psl/App.groovy"))
    try {
      pw.write("testing\n")
    } finally {
      pw.close()
    }
  }
}


