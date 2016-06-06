package utcompling.mlnsemantics.util

import org.apache.log4j.Level
import utcompling.Resources
import utcompling.scalalogic.discourse.impl.BoxerDiscourseInterpreter

/**
 * Input parameters
 * -vsWithPos true, (false)   //use vector space with POS or vector space without POS
 * -vectorMaker (add), mul    //vector composition, addition or multiplication
 * -chopLvl prp, (rp)         //chopping level: PredicateRelationPredicate or RelationPredicate
 * -dupPhraseLexical (true), false       //generated duplicates when generating Infernece rules
 * -scaleW (true), false      //Scale Weight when generating inference rules
 * -maxProb (0.93)            //max probablity
 * -wThr (0.35)               //cut off threshold for weights
 * -range (1-1500)            //pairs range
 * -log (DEBUG)               //log OFF, DEBUG, INFO
 * -varBind true, (false)     //with or without variable binding
 * -timeout 0  				  //Timeout integerInMilliseconds
 * -peInf (true) false          //include or execlude patient and agent infernece rules when irLvl = 2
 * -irLvl 0 1 (2)             //infernec rules: 0)no infernece rules, 1)word-wise infernec rules, 2)words and phrases infenrec rules
 * -logic dep (box)           //get logical form from Boxer or Dependency parse
 * -kbest 3                    //number of parses. Default: 1
 * -task sts					//sts, or rte. Default: sts
 * -softLogic (mln) psl			//use PSL or MLN. Default: MLN
 * -keepUniv false (true)		//keep univ quantifiers, or replace them with Exist. Default: true (keep them)
 */

class Config(opts: Map[String, String] = Map()) {

  val validArgs:List[String] = List(
	    "-log",
		"-timeout",
		"-ssTimeout",
		"-vectorSpace",
		"-distWeight",
		"-distRulesMode",
		"-phrases",
		"-genPhrases",
		"-phraseVecs",
		"-rulesWeight",
		"-rules",
		"-rulesMatchLemma",		
		"-vectorMaker",
		"-vsWithPos",
		"-alpha",
		"-ngramN",
		"-dupPhraseLexical",
		"-irLvl",
		"-wThr",
		"-lexInferModelFile",
		"-lexInferMethod",
		"-wordnet",
		"-graphRules",
		"-graphRuleLengthLimit",
		"-diffRules",
		"-printDiffRules",
		"-diffRulesSimpleText",
		"-extendDiffRulesLvl",
		"-splitDiffRules",
		"-task",
		"-varBind",
		"-chopLvl",
		"-maxProb",
		"-scaleW",
		"-logOddsW",
		"-logic",
		"-soap",
		"-fixDCA",
		"-noHMinus",
		"-keepUniv",
		"-lhsOnlyIntro",
		"-softLogic",
		"-withEventProp",
		"-negativeEvd",
		"-withNegT",
		"-groundExist",
		"-focusGround",
		"-groundLimit",
		"-funcConst",
		"-metaW",
		"-relW",
		"-kbest",
		"-multiOut",
		"-withExistence",
		"-withFixUnivInQ",
		"-withFixCWA",
		"-applyNegation",
		"-evdIntroSingleVar",
		"-prior",
		"-wFixCWA",
		"-ratio",
		"-coref",
		"-corefOnIR",
		"-errorCode", 
		"-removeEq", 
		"-ner",
		"-baseline",
		"-ruleClsModel",
		"-emRandInit",
		"-emTrainOnBest"
	);
  
  val diff = opts.keys.toSet.diff(validArgs.toSet)
  if(!diff.isEmpty)
  {
    throw new RuntimeException("Invalid arguments " + diff.mkString(", "));
  }
  
  //log levels as defined by log4j
  val loglevel = opts.get("-log").map(Level.toLevel).getOrElse(Level.OFF);
  
  var timeout = opts.get("-timeout") match {
  	case Some("-1") => None;  //no timeout 
  	case Some(t) => Some(t.toLong);
    case _ => Some(75000L);
  }
  //Timeout for SampleSearch inference
  var ssTimeout = opts.get("-ssTimeout") match {
  	case Some(t) => t.toInt;
    case _ => 5; //by default, timeout is 5 secoonds
  }

  //-------------------------------------------Precompiled distributional phrases (like Marco Baroni's phrases)

  // basic vector space
  var vectorSpace = opts.get("-vectorSpace") match {
    case Some(filename) => filename
    case _ => Resources.fullVectorSpace
  }

  // Scaling weights of distributional inference rules  
  val distWeight = opts.get("-distWeight") match {
    case Some(weight) => weight.toDouble
    case _ => 1.0
  }

  // Scaling weights of distributional inference rules  
  val distRulesMode = opts.get("-distRulesMode") match {
    case Some("gs") => "gs"  //use hard rules using the gold standard annotation  
    case Some("hard") => "hard"  //use best rule as hard rule
    case Some("weight") => "weight"  //use best rule as weighted rule
    case Some("noNeu") => "noNeu"  //use best rule as weighted rule excluding neutrals
    case None => "hard"
    case Some(x) => throw new RuntimeException("Invalid value " + x + " for argument -distRulesMode");
  }

  //file contains list of phrases (resources/phrases.lst)
  val phrasesFile = opts.get("-phrases") match {
    case Some(phrasesFile) => phrasesFile;
    case _ => "";
  }

  //file contains list of phrases (resources/phrases.lst)
  val genPhrases = opts.get("-genPhrases") match {
    case Some(genPhrases) => genPhrases.toBoolean;
    case _ => false;
  }
  
  // Named Entity Recognizer  
  val ner = opts.get("-ner") match {
    case Some("gs") => "gs"  //works only for the deepMind QA dataset  
    case Some("corenlp") => "corenlp"  //stanford coreNLP
    case Some("candc") => "candc"  //default
    case None => "candc"
    case Some(x) => throw new RuntimeException("Invalid value " + x + " for argument -ner");
  }
  if (ner != "candc")
    //do not use the default candc binary because it only accepts raw text, while here I need 
    //to pass POS and NER information with the text
    utcompling.scalalogic.discourse.candc.call.impl	.CandcImpl.binaryName = "candc.sh";

  

  //files contains vectors of these phrases (resources/phrase-vectors/word_phrase_sentence_*)
  val phraseVecsFile = opts.get("-phraseVecs") match {
    case Some(phraseVecsFile) => phraseVecsFile;
    case _ => "";
  }

  //-------------------------------------------Precompiled paraphrase dataset
  
  //Scaling weights of pre-compiled list of paraphrases 
  val rulesWeight = opts.get("-rulesWeight") match {
    case Some(weight) => weight.toDouble
    case _ => 1.0
  }
    
  //file contains paraphrase similarities (resources/rules)
  val rulesFile = opts.get("-rules") match {
    case Some(rulesFile) => rulesFile;
    case _ => "";
  }
  
  //Match paraphrases with the sentence or with the lemmatized sentnece ?
  val rulesMatchLemma = opts.get("-rulesMatchLemma") match {
    case Some(matchLemma) => matchLemma.toBoolean
    case _ => false;
  }

  //-------------------------------------------On-the-fly inference rules generation 
  
  //vector composition, addition or multiplication 
  val compositeVectorMaker = opts.get("-vectorMaker") match {
    case Some("ngram") => "ngram";
    case Some("mul") => "mul";
    case Some("add") => "add";
    case None => "add"; //default
    case Some(x) => throw new RuntimeException("Invalid value " + x + " for argument -vectorMaker");
  }
  
  //vector space has POS or not. The one I am using now is without POS. Gemma has ones with POS
  //The case of "with POS", is not well tested. Expect it to break 
  //Now the default is one with POS that Stephen built
  val vectorspaceFormatWithPOS = opts.get("-vsWithPos") match {
    case Some(vst) => vst.toBoolean
    case _ => true;
  }

  val ngramAlpha = opts.get("-alpha") match {
    case Some(v) => v.toDouble
    case _ => 0.8
  }

  val ngramN = opts.get("-ngramN") match {
    case Some(v) => v.toInt
    case _ => 4
  }

  //generate distributional phrasel inference rules for phrases including ones with agent and patient relations=
  //Not used anymore. Always include agent/patient rules
  //val withPatientAgentInferenceRules = opts.get("-peInf") match {
  //  case Some(vst) => vst.toBoolean;
  //  case _ => true;
  //}

  //Generate phrasal and lexical rules for the same predicate.  
  val duplicatePhraselAndLexicalRule = opts.get("-dupPhraseLexical") match {
    case Some(s) => s.toBoolean
    case _ => true;
  }
  
  //-------------------------------------------Arguments general for all inference rules
  
   //what level of inference rules to generate: 
  //-1)no rules at all  0)pre-compiled rules. No on-the-fly rules 1)pre-compiled rules + lexical rules, 2)all rules(precompiled, lexical, phrasal)
  val inferenceRulesLevel = opts.get("-irLvl") match {
    case Some(vst) => vst.toInt;
    case _ => 0;
  }
  
  //weight threshold
  val weightThreshold = opts.get("-wThr") match {
    case Some(thr) => thr.toDouble;
    case _ => 0.10;
  }

  val lexicalInferenceModelFile: String = opts.get("-lexInferModelFile") match {
    case Some(x) => x
    case _ => ""
  }


  // lexical inference method
  val lexicalInferenceMethod: utcompling.mlnsemantics.vecspace.LexEntailmentModel = opts.get("-lexInferMethod") match {
    case Some("cosine") => new utcompling.mlnsemantics.vecspace.CosineLexEntailmentModel
    case Some("asym") => new utcompling.mlnsemantics.vecspace.LogRegLexEntailmentModel(lexicalInferenceModelFile)
    case None => new utcompling.mlnsemantics.vecspace.CosineLexEntailmentModel
    case Some(x) => throw new RuntimeException("Invalid value " + x + " for argument -lexInferMethod");
  }

  //Enable or Disable WordNet rules
  val wordnet = opts.get("-wordnet") match {
    case Some("true") => true;
    case _ => false;
  }
  

  //Set graph rules level (same level used for the dep-baseline)
  val graphRules = opts.get("-graphRules") match {
  	case Some("0") => 0; //no rules 
  	case Some("1") => 1; //rules between placeholder and first-hop entities only
  	case Some("2") => 2; //rules for all relations in H
  	case Some("3") => 2; //rules between pairs: (placeholder, all other entities in H). Short rules are deleted from longer rules containing them
  	case None => 0; //no rules
    case Some(x) => throw new RuntimeException("Invalid value " + x + " for argument -graphRules");
  }
  
  //Limit of length of graph rules
  val graphRuleLengthLimit = opts.get("-graphRuleLengthLimit") match {
    case Some(len) => len.toInt;
    case _ => 5;
  }

  //Enable or Disable Difference rules
  val diffRules = opts.get("-diffRules") match {
    case Some("false") => false;
    case _ => true;
  }
  
  //Enable or Disable printing Difference rules
  val printDiffRules = opts.get("-printDiffRules") match {
    case Some("true") => true;
    case _ => false;
  }
  
 //if printing Difference rules is enabled, use simple text or full text 
  val diffRulesSimpleText = opts.get("-diffRulesSimpleText") match {
    case Some("false") => false;
    case _ => true;
  }
  
  //Enable or Disable extending Difference rules
  //None means, generate rules for the three levels of extension
  //Levels:
  // 0) no extension
  // 1) enhanced extension
  // 2) full extension
  var extendDiffRulesLvl = opts.get("-extendDiffRulesLvl") match {
  	case Some ("All") => None;
    case Some(lvl) => Some(lvl.toInt);
    case _ => Some(1);
  } 

  //Enable or Disable printing Difference rules
  val splitDiffRules = opts.get("-splitDiffRules") match {
    case Some("false") => false;
    case _ => true;
  }
  
  //-------------------------------------------task
    
  //task: rte, sts
  val task = opts.get("-task") match {
    case Some("sts") => "sts";
    case Some("rte") => "rte";
    case None => "rte";
    case Some(x) => throw new RuntimeException("Invalid value " + x + " for argument -task");
  }

  //variable binding (only on sts with mln)
  val varBind = opts.get("-varBind") match {
    case Some("true") => true;
    case _ => false;
  }

  //Chopping levels: type of mini-clauses (only on sts with mln)
  //rp (relation ^ predicate), prp (predicate ^ relation ^ predicate)
  val chopLvl = opts.get("-chopLvl") match {
    case Some("prp") => "prp";
    case Some("rp") => "rp";
    case None => "rp";
    case Some(x) => throw new RuntimeException("Invalid value " + x + " for argument -chopLvl");
  }

  //maximum probability. It is part of the equation to calculate the average-combiners's weights (only on sts with mln) 
  val maxProb = opts.get("-maxProb") match {
    case Some(prob) => prob.toDouble;
    case _ => 0.93;
  }

  //MLN splits weights on formulas. If scaleW is true, reverse this default MLN behaviour (only on mln)
  val scaleW = opts.get("-scaleW") match {
    case Some(s) => s.toBoolean
    case _ => true;
  }

  //Use the LogOdds function to map weights to MLN weights or use the weights as is
  val logOddsW = opts.get("-logOddsW") match {
    case Some(s) => s.toBoolean
    case _ => true;
  }

  //-------------------------------------------logic and inference

  val logicFormSource = opts.get("-logic") match {
    case Some("dep") => "dep";
    case Some("box") => "box";
    case None => "box";
    case Some(x) => throw new RuntimeException("Invalid value " + x + " for argument -logic");
  }

  val soapServer = opts.get("-soap") match {
    case Some(serverUrl) =>
      	if (ner != "candc")
      	  throw new RuntimeException("Using the soap server is only supported for the candc named entity recognizer");
		utcompling.scalalogic.discourse.candc.call.impl	.CandcImpl.binaryName = "soap_client";
		utcompling.scalalogic.discourse.candc.call.impl	.CandcImpl.extraArgs = Map (("--url" -> serverUrl))
    case None => None
  }

  //do all the changes required to fix the problems resulting from the Domain Closure Assumption
  //Setting it to "true" enables an old, wrong attempt to handle DCA
  //false means using the new correct code of handling DCA
  val fixDCA = opts.get("-fixDCA") match {
    case Some("true") => true;
    case _ => false;
  }

  //if fixDCA is true, noHMinus ignores H- and set negative prior on all predicates
  //if softLogic = "mln", noHMinus generates a query rule: H => entail instead of H <=> entail 
  val noHMinus = opts.get("-noHMinus") match {
    case Some("false") => false;
    case _ => true;
  }
  
  //keep universal quantifiers or replace them with existentials
  val keepUniv = opts.get("-keepUniv") match {
    case Some("false") => false;
    case _ => true;
  }
  
  //Introduction for all UNVs, or for LHS only
  val lhsOnlyIntro = opts.get("-lhsOnlyIntro") match {
    case Some("false") => false;
    case _ => true;
  }
  
  //What probabilistic logic tool to be used, PSL or MLN. NONE is a dummy inference that always returns 0. 
  val softLogicTool = opts.get("-softLogic") match {
    case Some("psl") => "psl"
    case Some("none") => "none"
    case Some("ss") => "ss"
    case Some("mln") => "mln"
    case None => {
      task match {
        case "rte" => "ss"
        case "sts" => "psl"
        case _ => "none"
      }
    }
    case Some(x) => throw new RuntimeException("Invalid value " + x + " for argument -softLogic");    
  }
  
  //recover event variables and prop variables or not
  val withEventProp = opts.get("-withEventProp") match {
    case Some("true") => true;
    case _ => false;
  }
  
  //recover event variables and prop variables or not
  //either generate negative evidnece (modified close-world assumption), or negative prior everywhere
 val negativeEvd = opts.get("-negativeEvd") match {
    case Some("false") => false;
    case _ => true;
  }

 //in RTE, find p(h|t) and p(h|-t) to help classify E,N,C
 val withNegT = opts.get("-withNegT") match {
    case Some("false") => false;
    case _ => true;
  }

 //with or without existance assumption 
 val withExistence = opts.get("-withExistence") match {
    case Some("false") => false;
    case _ => true;
  }

 //with or without fixing effect of DCA for Univ in Q
 val withFixUnivInQ = opts.get("-withFixUnivInQ") match {
    case Some("false") => false;
    case _ => true;
  }

 //with or without fixing effect of CWA  in Q
 var withFixCWA = opts.get("-withFixCWA") match {
    case Some("false") => false;
    case _ => true;
  }

 //replace NOT(A,B) with IMP(, NOT(B))  
 //This is in the parsing phase
 var applyNegation = opts.get("-applyNegation") match {
    case Some("true") => {
    	BoxerDiscourseInterpreter.applyNegation = true;
    	true;
    }
    case _ => false;
  }

 //remove one weird equalities
 var removeEq = opts.get("-removeEq") match {
    case Some("true") => true;
    case _ => false;
  }

 //with or without introduction of hard evidence for all universally 
 //quantified predicates with single variables. 
 //This is actually a hack because our code of detection of 
 //LHS and RHS or quantifiers is not complete
 //This hack is necesery for the synthetic dataset
 val evdIntroSingleVar = opts.get("-evdIntroSingleVar") match {
    case Some("true") => true;
    case _ => false;
  }

 //ground EXIST before calling alchemy
 val groundExist = opts.get("-groundExist") match {
    case Some("false") => false;
    case _ => true;
  }
 
 //Enable focus grounding; a theorem prover that finds non-inferrable ground atoms
 //The default is false which works with the RTE task (+ negativeEvd)
 //Change it to true for QA task 
 val focusGround= opts.get("-focusGround") match {
    case Some("true") => true;
    case _ => false;
  }

  //------------------------------------------PSL
  //PSL grounding limit. Set it to ZERO for "no limit"
  val groundLimit = opts.get("-groundLimit") match {
    case Some(l) => l.toInt;
    case _ => 10000; 
  }
  
  //partial functional  constraint on the agent and patient predicates
  val funcConst = opts.get("-funcConst") match {
     case Some("false") => false;
     case _ => true;
  }
  
  //weight of meta predicates, they are negationPred and dummyPred. 
  //"-1" means they should be treated as any other unary predicate.  
  val metaW = opts.get("-metaW") match {
     case Some(w) => w.toDouble;
     case _ => -1 //  0.30 gave the best result on the SICK-STS task
  }
  
  //weight of relation predicates like agent, patient, of ....  
  //"-1" means they should be treated as any other unary predicate.  
  val relW = opts.get("-relW") match {
     case Some(w) => w.toDouble;
     case _ => 0.05;  //-1
  }

  //-------------------------------------------multiple parses

  //number of parses
  val kbest = opts.get("-kbest") match {
    case Some(kbest) => kbest.toInt;
    case _ => 1;
  }

  //output files to store results of individual parses, when using multiple parses
  val multipleOutputFiles = opts.get("-multiOut") match {
    case Some(out) => out
    case _ => "multiOut"
  }
  
  //----------------------------------------------
  // Predicates prior in the RTE task   
  var prior = opts.get("-prior") match {
    case Some(weight) => weight.toDouble
    case _ => 0
  }

  // Weight of the FixCWA rule   
  var wFixCWA = opts.get("-wFixCWA") match {
    case Some(weight) => weight.toDouble
    case _ => 0.99
  }
  
  //detect contradiction through Ratio or notTGivenH
  val ratio = opts.get("-ratio") match {
     case Some("true") => true;
     case _ => false;
  }
  
  //doing coreference resolution between T and H 
  val coref = opts.get("-coref") match {
     case Some("false") => false;
     case _ => true;
  }
  //doing coreference resolution between T and H 
  val corefOnIR = opts.get("-corefOnIR") match {
     case Some("false") => false;
     case _ => true;
  }
  
  
  //Return system generated error codes, or an inputed error code 
  val errorCode = opts.get("-errorCode") match {
     case Some(x) => Some(x.toDouble);
     case _ => None;
  }
  def errorCode(x:Double):Double = {
  	if (x >=0) //no error
  		return x;
  	if(this.errorCode.isDefined)
  		return errorCode.get
  	return x
  }

  //QA baseline or the full system
  val baseline = opts.get("-baseline") match {
     case Some("word") => "word";
     case Some("dep") => "dep";
     case Some("full") => "full";
     case None => "full"
  }
  //Rule classifier trained model 
  val ruleClsModel = opts.get("-ruleClsModel") match {
     case Some(path) => Some(path);
     case _ => None;
  }
  
  //Init Expectation Maximization randomly or using reasonable initial values
  val emRandInit = opts.get("-emRandInit") match {
     case Some("true") => true;
     case Some("false") => false;
     case None => true;
  }
  
  //Rules to train Expectation Maximization are the best rules of every example, or all rules of every example 
  val emTrainOnBest = opts.get("-emTrainOnBest") match {
     case Some("true") => true;
     case Some("false") => false;
     case None => true;
  }
}
object Config {
	
}
