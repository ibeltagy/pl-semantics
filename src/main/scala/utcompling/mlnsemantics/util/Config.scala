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

  val loglevel = opts.get("-log").map(Level.toLevel).getOrElse(Level.DEBUG);

  // Weight of distributional inference rules
  val distWeight = opts.get("-distWeight") match {
    case Some(weight) => weight.toDouble
    case _ => 1.0
  }

  //Weight of external inference rules
  val resourceWeight = opts.get("-resourceWeight") match {
    case Some(weight) => weight.toDouble
    case _ => 1.0
  }

  //task: rte, sts
  val task = opts.get("-task") match {
    case Some("rte") => "rte";
    case _ => "sts";
  }

  val compositeVectorMaker = opts.get("-vectorMaker") match {
    case Some("mul") => "mul";
    case _ => "add";
  }

  val logicFormSource = opts.get("-logic") match {
    case Some("dep") => "dep";
    case _ => "box";
  }

  val softLogicTool = opts.get("-softLogic") match {
    case Some("psl") => "psl"
    case _ => "mln"
  }

  val vectorspaceFormatWithPOS = opts.get("-vsWithPos") match {
    case Some(vst) => vst.toBoolean;
    case _ => false;
  }

  //file contains phrases similarities
  val phrasesFile = opts.get("-phrases") match {
    case Some(phrasesFile) => phrasesFile;
    case _ => "";
  }

  val phraseVecsFile = opts.get("-phraseVecs") match {
    case Some(phraseVecsFile) => phraseVecsFile;
    case _ => "";
  }

  //file contains paraphrase similarities
  val rulesFile = opts.get("-rules") match {
    case Some(rulesFile) => rulesFile;
    case _ => "";
  }

  //variable binding
  val varBind = opts.get("-varBind") match {
    case Some("true") => Some(true);
    case _ => Some(false);
  }

  //weight threshold
  val weightThreshold = opts.get("-wThr") match {
    case Some(thr) => thr.toDouble;
    case _ => 0.20;
  }

  //Chopping levels: type of mini-clauses
  //rp (relation ^ predicate), prp (predicate ^ relation ^ predicate)
  val chopLvl = opts.get("-chopLvl") match {
    case Some("prp") => "prp";
    case _ => "rp";
  }

  //maximum probability. It is part of the equation to calculate the average-combiners's weights 
  val maxProb = opts.get("-maxProb") match {
    case Some(prob) => prob.toDouble;
    case _ => 0.93;
  }

  //MLN splits weights on formulas. If scaleW is true, reverse this default MLN behaviour
  val scaleW = opts.get("-scaleW") match {
    case Some(s) => s.toBoolean
    case _ => true;
  }

  val timeout = opts.get("-timeout") match {
    case Some(t) => Some(t.toLong);
    case _ => None;
  }

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

  //keep universal quantifiers or replace them with existentials
  val keepUniv = opts.get("-keepUniv") match {
    case Some("false") => false;
    case _ => true;
  }

  //what level of inference rules to generate: 
  //0)no infernece rules, 1)word-wise infernec rules, 2)words and phrases infenrec rules
  val inferenceRulesLevel = opts.get("-irLvl") match {
    case Some(vst) => vst.toInt;
    case _ => 2;
  }
  //generate distributional phrasel inference rules for phrases including ones with agent and patient relations= 
  val withPatientAgentInferenceRules = opts.get("-peInf") match {
    case Some(vst) => vst.toBoolean;
    case _ => true;
  }

  //Generate phrasal and lexical rules for the same predicate.  
  val duplicatePhraselAndLexicalRule = opts.get("-dupPhraseLexical") match {
    case Some(s) => s.toBoolean
    case _ => true;
  }

}

object Config {

}