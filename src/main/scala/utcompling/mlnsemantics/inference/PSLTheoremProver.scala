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

class PSLTheoremProver(
  override val binary: String,
  prior: Double = -3,
  var entWeight: Double = 1,
  logBase: Double = E)
  extends SubprocessCallable(binary)
  with ProbabilisticTheoremProver[FolExpression] {

  type WeightedFolEx = WeightedExpression[FolExpression]

  private val LOG = LogFactory.getLog(classOf[PSLTheoremProver])


  private var entailmentConsequent_h:FolExpression = FolVariableExpression(Variable("entailment_h")); 
  private var entailmentConsequent_t:FolExpression = FolVariableExpression(Variable("entailment_t"));
  private val entailedConst = ("ent" -> Set("ent_h", "ent_t"))
  private var entailmentConsequent:FolExpression = FolAtom(Variable("entailment_h"), Variable(""));
  
  private var varBind: Option[Boolean] = None;
  private var task = "sts";
  
  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[FolExpression, Seq[String]],
    evidence: List[FolExpression],
    assumptions: List[WeightedFolEx],
    goal: FolExpression): Option[Double] = {

    if (varBind == None)
    	varBind  = Sts.opts.get("-varBind") match {
			case Some("true") => Some(true);
			case _ => Some(false);
		}
    else varBind  = Some(false); //second call
    
    task = Sts.opts.get("-task") match {
		case Some(tsk) => tsk;
		case _ => "sts";
      }
    
    
    declarations.foreach { dec =>
      dec match {
        case (FolAtom(Variable(pred), args @ _*), argTypes) =>
          for (a <- argTypes)
            require(constants.contains(a), "No contants were found for type '%s' of declared predicate '%s'.".format(a, pred))
        case d => throw new RuntimeException("Only atoms may be declared.  '%s' is not an atom.".format(d))
      }
    }
    
    // This block adds all variables to the entailment clause. 
    val variables: Set[Variable] = findAllVars(goal);
    var typeParam_h : List[String]= List();
    var typeParam_t : List[String]= List();
    if(!varBind.get){
	    typeParam_h = List();
	    typeParam_t = List();
	    entailmentConsequent_t = FolAtom(Variable("entailment_t"), Variable("")); 
	    entailmentConsequent_h = FolAtom(Variable("entailment_h"), Variable(""));
    }
    else
    {
	    for (v <- variables )
	    {
	     	  v.name.charAt(0) match {
	     	    case 't' => entailmentConsequent_t = FolApplicationExpression(entailmentConsequent_t, FolVariableExpression(Variable(v.name))); 
	     	    case 'h' => entailmentConsequent_h = FolApplicationExpression(entailmentConsequent_h, FolVariableExpression(Variable(v.name)));
	     	    case _ => throw new RuntimeException ("unsupported type");
	     	  }
	     	  v.name.substring(0, 2) match {
	     	    case "tx" => typeParam_t ::= "indv_t";
	     	    case "te" => typeParam_t ::= "evnt_t";
	     	    case "tp" => typeParam_t ::= "prop_t";
	     	    case "hx" => typeParam_h ::= "indv_h";
	     	    case "he" => typeParam_h ::= "evnt_h";
	     	    case "hp" => typeParam_h ::= "prop_h";
	     	    case _ => throw new RuntimeException ("unsupported type");
	     	  } 
	    }
    }
    typeParam_h = typeParam_h.reverse;
    typeParam_t = typeParam_t.reverse;
    val entailedDec_h = FolVariableExpression(Variable("entailment_h")) -> typeParam_h;
    val entailedDec_t = FolVariableExpression(Variable("entailment_t")) -> typeParam_t;
 
    
    val declarationNames =
      (declarations + entailedDec_h + entailedDec_t).mapKeys {
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
    	//Process("mvn", Seq("compile", "-f", "psl/pom.xml")) ! (ProcessLogger(System.err.println(_), System.err.println(_)))
    	//Process("mvn", Seq("exec:java", "-Dexec.mainClass=psl.App", "-f", "psl/pom.xml")) ! (ProcessLogger(System.err.println(_), System.err.println(_)))
    	return Some(-1.0);
    	//callAlchemy(mlnFile, evidenceFile, resultFile, args) match {
	    //  case Some(t) => Some(t.toDouble)
	    //  case _ => throw new RuntimeException("no valid output in the result file");
    	//}
    }catch 
    {
    	case e: Exception =>{
    	  System.err.println (e);
    	  if (varBind.get) //try again 
    	  {
			 println("backoff to dependency parse")
    	    return this.prove(constants, declarations, evidence, assumptions, goal);
    	  }
    	  else return Some(-1.0);
    	}   				 
    }
    
  }

  private def makeMlnFile(
    constants: Map[String, Set[String]],
    declarationNames: Map[String, Seq[String]],
    assumptions: List[WeightedFolEx],
    evidence: List[FolExpression],
    goal: FolExpression) = {
    
    PSLTheoremProver.pairIndx = PSLTheoremProver.pairIndx + 1;
    val pslFile = new java.io.PrintWriter(new File("psl/run/%s.psl".format(PSLTheoremProver.pairIndx)))
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
      pslFile.writeLine("predicate,all,1")
		declarationNames.foreach {
			case (pred, varTypes) => {
				//pslFile.writeLine("m.add predicate: \"%s\", %s open: true;".format(pred, varTypes.indices.map("arg" + _+": Entity, ").mkString("")))
			  pslFile.writeLine("predicate,%s,%s".format(pred, varTypes.length))
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
		    case ("entailment_h", varTypes) => pslFile.writeLine("prior,%s,0.01".format("entailment_h"))
			case ("entailment_t", varTypes) => pslFile.writeLine("prior,%s,0.01".format("entailment_t"))
			case ("entailment", varTypes) => pslFile.writeLine("prior,%s,0.01".format("entailment"))
			case (pred, varTypes) => pslFile.writeLine("prior,%s,0.1".format(pred))
		}
		
       //=================Inference rules		
		val weightThreshold  = Sts.opts.get("-wThr") match {
			case Some(thr) => thr.toDouble;
			case _ => 0.0;
		}
		
		var similarityTable: List[(String, Double)] = List(); 
		var lastSimilarityID:Integer = 0;
		assumptions
        .flatMap {
          case e @ SoftWeightedExpression(folEx, weight) =>
            weight match {
              case Double.PositiveInfinity => Some(HardWeightedExpression(folEx))
              case Double.NegativeInfinity => None ;//Some(HardWeightedExpression(-folEx))
              case _ if weight < weightThreshold => None
              case _ => Some(e)
            }
          case e @ HardWeightedExpression(folEx) => Some(e)
        }.foreach { e => 
          e match {
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
		              }.mkString("&")
		              
		              val rhsAnds = getAnds(rhs)
		              val rhsSimString = rhsAnds.flatMap {
		              	case FolAtom(pred, args @ _*) if (args.length == 1)=> List(pred.name)
		              	case _ =>None
		              }.mkString("&")
		              
		              //similarityTable ::= (lhsSimString+"#"+rhsSimString, usedWeight) ;
		              lastSimilarityID = lastSimilarityID+1;
		              similarityTable ::= (lastSimilarityID.toString(), usedWeight) ;
		              
		              rhsAnds.foreach(rhsAnd => {
		            	  val rhsVar = findAllVars(rhsAnd)
		            	  val missingVars = (rhsVar &~ lhsVars).toSet;
		            	  var extendedLhsString = lhsString;
		            	  missingVars.foreach(v => {
		            	     //extendedLhsString = "(%s & all(%s))".format(extendedLhsString, v.name.toUpperCase())
		            	    extendedLhsString = "%s&all(%s)".format(extendedLhsString, v.name.toUpperCase())
		            	  })
		            	  val rhsString = convert(rhsAnd)
		            	  //pslFile.writeLine("m.add rule: (%s & sim(\"%s\", \"%s\")) >> %s, constraint: true;"
		            	  pslFile.writeLine("rule,%s&sim(\"%s\",\"%s\")>>%s"
		            	      .format(extendedLhsString, lastSimilarityID.toString(), "", rhsString))
		              })	                  
	                }
	                case _ => throw new RuntimeException("unsupported infernece rule format"); 
	              }
	            }
	          case HardWeightedExpression(folExp) => throw new RuntimeException("only simple inference rules are accepted");
         }
       }
        
       //=================Goal
       task match {
      	//case "rte" => pslFile.writeLine("m.add rule: %s, constraint: true;".format(convert(universalifyGoalFormula(goal -> entailmentConsequent)))) //normal anding
         case "rte" => pslFile.writeLine("rule,%s".format(convert(universalifyGoalFormula(goal -> entailmentConsequent)))) //normal anding
      	//case "sts" => pslFile.writeLine("m.add rule: %s, constraint: true;".format(convert(universalifyGoalFormula(goal -> entailmentConsequent)))) //normal anding
         case "sts" => pslFile.writeLine("rule,%s".format(convert(universalifyGoalFormula(goal -> entailmentConsequent)))) //normal anding
       }
       

       //=================Similarity File      
         val simFile = new java.io.PrintWriter(new File("psl/run/%s.sim".format(PSLTheoremProver.pairIndx)))
         similarityTable.foreach(simEntry =>{
    	   simFile.write("%s,%s\n".format(simEntry._1, simEntry._2))
    	})
    	simFile.close();
    	   
       

       //=================Evidences
       //pslFile.write(
	   //	"DataStore data = new RelationalDataStore(m);\n" +
	   //	"data.setup db : DatabaseDriver.H2;\n");
		
	     evidence.foreach {
	        case e @ FolAtom(pred, args @ _*) => 
	          		pslFile.writeLine(
	          		    //"data.getInserter(%s).insert(%s);".format(pred.name, args.map(a => {
	          		    "data,%s,%s".format(pred.name, args.map(a => {
	          					a.name.substring(2).toInt+1000*min(a.name.charAt(0).toLower - 103, 2)
	          			}).mkString(","))
	          			);
	        case e => throw new RuntimeException("Only atoms may be evidence.  '%s' is not an atom.".format(e))
	    }
       //=================Query
		pslFile.writeLine(
		    //"ConfigManager cm = ConfigManager.getManager();\n" +
		    //"ConfigBundle exampleBundle = cm.getBundle(\"example\");\n" +
		    //"def result = m.mapInference(data.getDatabase(), exampleBundle);\n" +

		    //"def result = m.mapInference(data.getDatabase());\n" +
		    //"result.printAtoms(entailment_h, false);")
		    "query,entailment_h")
    
	   pslFile.close();
    }
    "hi";
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
      //case FolAndExpression(first, second) => "(" + _convert(first, bound) + " & " + _convert(second, bound) + ")"
      case FolAndExpression(first, second) => _convert(first, bound) + "&" + _convert(second, bound) 
      case FolOrExpression(first, second) => "(" + _convert(first, bound) + " v " + _convert(second, bound) + ")"
      //case FolIfExpression(first, second) => "(" + _convert(first, bound) + " >> " + _convert(second, bound) + ")"
      case FolIfExpression(first, second) =>  _convert(first, bound) + ">>" + _convert(second, bound) 
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
      //case FolAtom(pred, args @ _*) => pred.name.replace("'", "") + "(" + args.map(v => if (bound(v)) v.name.toLowerCase() else quote(v.name)).mkString(",") + ")"
      case FolAtom(pred, args @ _*) => pred.name.replace("'", "") + "(" + args.map(v => v.name.toUpperCase()).mkString(",") + ")"
      case FolVariableExpression(v) => v.name.toUpperCase()//if (bound(v)) v.name.toLowerCase() else quote(v.name)
    }

  private def quote(s: String) = '"' + s + '"'

}

object PSLTheoremProver {

  private var pairIndx = 0;

  def findBinary(binDir: Option[String] = None, envar: Option[String] = Some("PSLHOME"), verbose: Boolean = false) =
  {
    //new PSLTheoremProver(FileUtils.findBinary("infer", binDir, envar, verbose))
    new PSLTheoremProver("")
  }

  def main(args: Array[String]) {

    Process("mvn", Seq("compile", "-f", "psl/pom.xml")) ! (ProcessLogger(System.err.println(_), System.err.println(_)))
    Process("mvn", Seq("exec:java", "-Dexec.mainClass=psl.Template", "-f", "psl/pom.xml")) ! (ProcessLogger(System.err.println(_), System.err.println(_)))
    
    val pw = new java.io.PrintWriter(new File("psl/src/main/java/psl/App.groovy"))
    try {
      pw.writeLine("testing")
    } finally {
      pw.close()
    }
  }
}
