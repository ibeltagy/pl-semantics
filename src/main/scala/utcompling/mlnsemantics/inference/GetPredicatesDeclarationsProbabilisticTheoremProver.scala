package utcompling.mlnsemantics.inference

import edu.mit.jwi.item.POS
import scala.collection.JavaConversions._
import scala.collection.mutable.SetBuilder
import utcompling.mlnsemantics.inference.support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.mlnsemantics.wordnet.Wordnet
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import org.apache.commons.logging.LogFactory
import support.HardWeightedExpression

class GetPredicatesDeclarationsProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {  
  
  private val LOG = LogFactory.getLog(classOf[GetPredicatesDeclarationsProbabilisticTheoremProver])

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {
  
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


    var txtEx = assumptions.head.expression;
    var hypEx = goal;
    
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
    val newAssumptions = List(HardWeightedExpression(txtEx))
    val newGoal = hypEx
    delegate.prove(constants, declarations, evidence, newAssumptions, newGoal)
  } 
}