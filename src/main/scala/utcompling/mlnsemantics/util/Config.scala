package utcompling.mlnsemantics.util

import org.apache.log4j.Level

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

  //log levels as defined by log4j
  val loglevel = opts.get("-log").map(Level.toLevel).getOrElse(Level.OFF);
  
  val timeout = opts.get("-timeout") match {
  	case Some("-1") => None;  //no timeout 
  	case Some(t) => Some(t.toLong);
    case _ => Some(120000L); //by default, timeout is 5 minutes
  }

  //-------------------------------------------Precompiled distributional phrases (like Marco Baroni's phrases)

  // basic vector space
  val vectorSpace = opts.get("-vectorSpace") match {
    case Some(filename) => filename
    case _ => "resources/full.vs"
  }

  // Scaling weights of distributional inference rules  
  val distWeight = opts.get("-distWeight") match {
    case Some(weight) => weight.toDouble
    case _ => 1.0
  }

  //file contains list of phrases (resources/phrases.lst)
  val phrasesFile = opts.get("-phrases") match {
    case Some(phrasesFile) => phrasesFile;
    case _ => "";
  }

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

  //-------------------------------------------On-the-fly inference rules generation 
  
  //vector composition, addition or multiplication 
  val compositeVectorMaker = opts.get("-vectorMaker") match {
    case Some("ngram") => "ngram";
    case Some("mul") => "mul";
    case _ => "add";
  }
  
  //vector space has POS or not. The one I am using now is without POS. Gemma has ones with POS
  //The case of "with POS", is not well tested. Expect it to break 
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
    case _ => 2;
  }
  
  //weight threshold
  val weightThreshold = opts.get("-wThr") match {
    case Some(thr) => thr.toDouble;
    case _ => 0.20;
  }

  val lexicalInferenceModelFile: String = opts.get("-lexInferModelFile") match {
    case Some(x) => x
    case _ => ""
  }


  // lexical inference method
  val lexicalInferenceMethod: utcompling.mlnsemantics.vecspace.LexEntailmentModel = opts.get("-lexInferMethod") match {
    case Some("cosine") => new utcompling.mlnsemantics.vecspace.CosineLexEntailmentModel
    case Some("asym") => new utcompling.mlnsemantics.vecspace.LogRegLexEntailmentModel(lexicalInferenceModelFile)
    case _ => new utcompling.mlnsemantics.vecspace.CosineLexEntailmentModel
  }

  
  //-------------------------------------------task
    
  //task: rte, sts
  val task = opts.get("-task") match {
    case Some("sts") => "sts";
    case _ => "rte";
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
    case _ => "rp";
  }

  //maximum probability. It is part of the equation to calculate the average-combiners's weights (only on sts with mln) 
  val maxProb = opts.get("-maxProb") match {
    case Some(prob) => prob.toDouble;
    case _ => 0.93;
  }

  //MLN splits weights on formulas. If scaleW is true, reverse this default MLN behaviour (only on mln)
  val scaleW = opts.get("-scaleW") match {
    case Some(s) => s.toBoolean
    case _ => false;
  }

  //-------------------------------------------logic and inference

  val logicFormSource = opts.get("-logic") match {
    case Some("dep") => "dep";
    case _ => "box";
  }

  //do all the changes required to fix the problems resulting from the Domain Closure Assumption
  val fixDCA = opts.get("-fixDCA") match {
    case Some("false") => false;
    case _ => true;
  }

  //if fixDCA is true, noHMinus ignores H- and set negative prior on all predicates
  val noHMinus = opts.get("-noHMinus") match {
    case Some("false") => false;
    case _ => true;
  }
  
  //keep universal quantifiers or replace them with existentials
  val keepUniv = opts.get("-keepUniv") match {
    case Some("false") => false;
    case _ => true;
  }
  
  //What probabilistic logic tool to be used, PSL or MLN. NONE is a dummy inference that always returns 0. 
  val softLogicTool = opts.get("-softLogic") match {
    case Some("psl") => "psl"
    case Some("none") => "none"
    case Some("ss") => "ss"
    case Some("mln") => "mln"
    case _ => {
      task match {
        case "rte" => "ss"
        case "sts" => "psl"
        case _ => "none"
      }
    }
  }
  
  //recover event variables and prop variables or not
  val withEventProp = opts.get("-withEventProp") match {
    case Some("true") => true;
    case _ => false;
  }
  
  //recover event variables and prop variables or not
 val negativeEvd = opts.get("-negativeEvd") match {
    case Some("false") => false;
    case _ => true;
  }

 //in RTE, find p(h|t) and p(h|-t) to help classify E,N,C
 val withNegT = opts.get("-withNegT") match {
    case Some("false") => false;
    case _ => true;
  }
 
 //ground EXIST before calling alchemy
 val groundExist = opts.get("-groundExist") match {
    case Some("false") => false;
    case _ => true;
  }
 
 //Enable focus grounding. It does full grounding taking 
 //wrong types into account  
 val focusGround= opts.get("-focusGround") match {
    case Some("false") => false;
    case _ => true;
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
     case _ => 0.33;  //-1
  }
  
  //weight of relation predicates like agent, patient, of ....  
  //"-1" means they should be treated as any other unary predicate.  
  val relW = opts.get("-relW") match {
     case Some(w) => w.toDouble;
     case _ => 0.01;  //-1
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
  
}

object Config {

}
