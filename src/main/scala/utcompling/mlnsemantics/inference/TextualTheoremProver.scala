package utcompling.mlnsemantics.inference

import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import opennlp.scalabha.util.FileUtils.pathjoin
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import utcompling.scalalogic.inference.TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.UnnecessarySubboxRemovingBoxerExpressionInterpreter
import utcompling.scalalogic.drt.expression.parse.DrtLogicParser
import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.FolApplicationExpression
import utcompling.scalalogic.fol.expression.FolVariableExpression
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SetBuilder
import utcompling.scalalogic.fol.expression.FolAtom
import org.apache.log4j.Logger
import org.apache.log4j.Level
import utcompling.mlnsemantics.wordnet.WordnetImpl
import utcompling.mlnsemantics.inference.support._
import utcompling.scalalogic.discourse.DiscourseInterpreter

class TextualTheoremProver(
  discourseIterpreter: DiscourseInterpreter[BoxerExpression],
  probabilisticTheoremProver: ProbabilisticTheoremProver[BoxerExpression]) {

  def prove(text: String, hyp: String): Option[Double] =
    prove(List(text), List(hyp))

  def prove(text: List[String], hyp: List[String]): Option[Double] = {
    val List(t, h) = discourseIterpreter.batchInterpretMultisentence(List(text, hyp), Some(List("t", "h")), false, false)

    var txtEx  = (t match {
      case Some(txt) => txt;
      case _ => {
        println ("Parsing failed. Return 0.5");
        return Some (0.5);
      }
    })
    
    var hypEx  = (h match {
      case Some(txt) => txt;
      case _ => {
        println ("Parsing failed. Return 0.5");
        return Some (0.5);
      }
    })

    //List(Some(txtEx), Some(hypEx))
    
    val IndvVar = """^(x\d*)$""".r
    val EvntVar = """^(e\d*)$""".r
    val PropVar = """^(p\d*)$""".r

    def combinePredicatesAndArgTypes(list: List[(Map[BoxerExpression, List[String]], Map[String, Set[String]])]): (Map[BoxerExpression, List[String]], Map[String, Set[String]]) = {
      val (predTypes, constTypes) = list.unzip
      val combinedPredTypes =
        predTypes.flatten.groupByKey.map {
          case (k, head :: tail) =>
            tail.foreach(t => (head zipSafe t).foreach { case (a, b) => assert(a == b, k + ": " + (head :: tail)) })
            (k, head)
        }
      val combinedConstTypes = constTypes.flatten.groupByKey.mapVals(_.flatten.toSet)
      (combinedPredTypes, combinedConstTypes)
    }

    def getPredicatesAndArgTypes(e: BoxerExpression): (Map[BoxerExpression, List[String]], Map[String, Set[String]]) =
      e match {
        case BoxerPred(discId, indices, variable, name, pos, sense) =>{
          val res1 = _getPredAndArgTypesTypes(e, List(variable), discId)
          val e2 = BoxerPred(discId match {case "t" => "h"; case "h" => "t"}, indices, variable, name, pos, sense);
          val res2 = _getPredAndArgTypesTypes(e2, List(variable), e2.discId)
          return (res1._1 ++ res2._1, res1._2 ++ res2._2)
        }
        case BoxerNamed(discId, indices, variable, name, typ, sense) =>{
          val res1 = _getPredAndArgTypesTypes(e, List(variable), discId)
          val e2 = BoxerNamed(discId match {case "t" => "h"; case "h" => "t"}, indices, variable, name, typ, sense);
          val res2 = _getPredAndArgTypesTypes(e2, List(variable), e2.discId)
          return (res1._1 ++ res2._1, res1._2 ++ res2._2)
        }
        case BoxerRel(discId, indices, event, variable, name, sense) =>{          
          if (name == "theme") println(e)
          val res1 = _getPredAndArgTypesTypes(e, List(event, variable), discId)
          val e2 = BoxerRel(discId match {case "t" => "h"; case "h" => "t"}, indices, event, variable, name, sense);
          val res2 = _getPredAndArgTypesTypes(e2, List(event, variable), e2.discId)
          return (res1._1 ++ res2._1, res1._2 ++ res2._2)
        }
        case BoxerCard(discId, indices, variable, num, typ) =>
          val res1 = _getPredAndArgTypesTypes(e, List(variable), discId)
          val e2 = BoxerCard(discId match {case "t" => "h"; case "h" => "t"}, indices, variable, num, typ);
          val res2 = _getPredAndArgTypesTypes(e2, List(variable), e2.discId)
          return (res1._1 ++ res2._1, res1._2 ++ res2._2)
        
        case BoxerTimex(discId, indices, variable, timeExp) =>
          val res1 = _getPredAndArgTypesTypes(e, List(variable), discId)
          val e2 = BoxerTimex(discId match {case "t" => "h"; case "h" => "t"}, indices, variable, timeExp);
          val res2 = _getPredAndArgTypesTypes(e2, List(variable), e2.discId)
          return (res1._1 ++ res2._1, res1._2 ++ res2._2)
        
        //case BoxerDate(indicesPol, pol, indicesYear, year, indicesMonth, month, indicesDay, day) =>
         // _getPredAndArgTypesTypes(e, List(BoxerVariable("d")), "")
          
        case _ => {
          e.visit(getPredicatesAndArgTypes, combinePredicatesAndArgTypes, (Map[BoxerExpression, List[String]](), Map[String, Set[String]]()))
        }
      }

    def _getPredAndArgTypesTypes(name: BoxerExpression, args: List[BoxerVariable], txtOrHyp: String): (Map[BoxerExpression, List[String]], Map[String, Set[String]]) = {
      val (argTypes, constants) =
        args.foldLeft(List[String](), List[(String, String)]()) {
          case ((argTypes, constants), v) =>
            v.name match {
              case IndvVar(v) => ("indv_"+txtOrHyp :: argTypes, constants)
              case EvntVar(v) => ("evnt_"+txtOrHyp :: argTypes, constants)
              case PropVar(v) => ("prop_"+txtOrHyp :: argTypes, constants)
              case c => ("indv_"+txtOrHyp :: argTypes, "indv_"+txtOrHyp -> c :: constants)
            }
        }
      val predTypes = Map(name -> argTypes.reverse)
      val constTypes = constants.toSet.groupByKey
      (predTypes, constTypes)
    }

    var eventVars = findEventVar(txtEx);
    txtEx = convertToEvntVar(txtEx);

    eventVars = findEventVar(hypEx);
    hypEx = convertToEvntVar(hypEx);
    
    
    def convertToEvntVar(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerRel(discId, indices, event, variable, name, sense) =>{
        	if (name == "agent" || name == "patient")
        		BoxerRel(discId, indices, convertVarToEvntVar(event), variable, name, sense);
        	else
        	  e        	  
        }
        case BoxerPred(discId, indices, variable, name, pos, sense) => 
          			BoxerPred(discId, indices, convertVarToEvntVar(variable), name, pos, sense)
        case BoxerDrs (ref, cond) => BoxerDrs (ref.map ( (listRef:(List[BoxerIndex], BoxerVariable)) =>{
          (listRef._1, convertVarToEvntVar(listRef._2))
        } ), cond.map (convertToEvntVar));
        case BoxerCard(discId, indices, variable, num,typ) =>
          			BoxerCard(discId, indices, convertVarToEvntVar(variable), num,typ)
        case BoxerProp(discId, indices, variable, drs) => 
          				BoxerProp(discId, indices, convertVarToEvntVar(variable), convertToEvntVar(drs))
        case BoxerEq(discId, indices, first, second) => BoxerEq(discId, indices, convertVarToEvntVar(first), convertVarToEvntVar(second))
        case BoxerNamed(discId, indices, variable, name, typ, sense) => BoxerNamed(discId, indices, convertVarToEvntVar(variable), name, typ, sense)
        case BoxerTimex(discId, indices, variable, timeExp) => BoxerTimex(discId, indices, convertVarToEvntVar(variable), timeExp);
        case _ => e.visitConstruct(convertToEvntVar)
      }
    }
    
    def convertVarToEvntVar(v: BoxerVariable): BoxerVariable = {
    	if (eventVars.contains(v))
    		return BoxerVariable("e" + v.name.substring(1))
    	else 
    		return v;
    }
    
    var txtExPredArg = getPredicatesAndArgTypes(txtEx)
    var hypExPredArg = getPredicatesAndArgTypes(hypEx)
    //if there is a duplicate predicate declaration, remove the one generated from the other sentence
    for (tPred <- txtExPredArg._1)
    {
      var tName = "";
      var tDiscId= "";
      var tPos= "";
      tPred._1 match {
        case BoxerPred(discId, indices, variable, name, pos, sense) => tName = name; tDiscId = discId; tPos = pos;
        case BoxerRel(discId, indices, event, variable, name, sense) => tName = name; tDiscId = discId;
        case _ => tDiscId = "t";
      }
      
      for (hPred <- hypExPredArg._1)
      {
	      var hName = "";
	      var hDiscId= "";
	      var hPos= "";
	      hPred._1 match {
	        case BoxerPred(discId, indices, variable, name, pos, sense) => hName = name; hDiscId = discId; hPos = pos;
	        case BoxerRel(discId, indices, event, variable, name, sense) => hName = name; hDiscId = discId;
	        case _ => hDiscId = "h";
	      }
	      
	      //same name, same discIf, same pos -> conflict so remove one of them
	      if (tName == hName && tDiscId == hDiscId && tPos == hPos)
	      {
	        //Dubplicate. Delete one of them
	        if (tDiscId == "h")
	          //delete from txt
	        	txtExPredArg = (txtExPredArg._1 - tPred._1) -> txtExPredArg._2 
	        else if (hDiscId == "t")
	          //delete from hyp
	          	hypExPredArg = (hypExPredArg._1 - hPred._1) -> hypExPredArg._2
	        else throw	new Exception ("Unknown error while generating predicates declarations")
	      }
      }
      
    }
    val (predTypes, constTypes) = combinePredicatesAndArgTypes(List(txtExPredArg, hypExPredArg))
    //val (predTypes, constTypes) = combinePredicatesAndArgTypes(List(txtEx, hypEx).map(getPredicatesAndArgTypes))
    
    val constants =
      Map(
        "indv_h" -> Set("default_indv_h_variable"),
        "evnt_h" -> Set("default_evnt_h_variable"),
        "prop_h" -> Set("default_prop_h_variable"), 
        "indv_t" -> Set("default_indv_t_variable"),
        "evnt_t" -> Set("default_evnt_t_variable"),
        "prop_t" -> Set("default_prop_t_variable")) ++ constTypes
    val declarations = predTypes //List("man(ind)", "mortal(ind)")
    val evidence = List() //"man(socrates)"
    val assumptions = List(HardWeightedExpression(txtEx))
    val goal = hypEx
    probabilisticTheoremProver.prove(constants, declarations, evidence, assumptions, goal)

  }
  
   private def findEventVar(e: BoxerExpression): List[BoxerVariable] = {
      e match {
        case BoxerRel(discId, indices, event, variable, name, sense) => {
          if (name == "agent" || name == "patient")
        	  return List(event);
          else 
            return List();
        }
        case _ => {
          e.visit(findEventVar, (parts: List[List[BoxerVariable]]) => {
            var compined = List[BoxerVariable]();
            for(p <- parts)
              compined  = compined ++ p;
            return compined.toSet.toList
          }  ,List())
        }
      }
   }
   
  
}
