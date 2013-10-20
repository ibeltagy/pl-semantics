package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.inference.support.WeightedExpression
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.CollectionUtils._
import support.HardWeightedExpression
import utcompling.mlnsemantics.inference.support.SoftWeightedExpression
import utcompling.mlnsemantics.run.Sts

object SetVarBindPTP{
  var entPred_h: FolExpression = null
  var entPred_t: FolExpression = null
}
    
class SetVarBindPTP(
  delegate: ProbabilisticTheoremProver[FolExpression])
  extends ProbabilisticTheoremProver[FolExpression] {

  def prove(
    constants: Map[String, Set[String]], // type -> constant
    declarations: Map[FolExpression, Seq[String]], // predicate -> seq[type] 
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Option[Double] = {

    //Construct the entailment predicate and its declaration. It depends on if we are doing variable binding or not. 
    def getEntDeclar(varBind: Boolean): Map[FolExpression, Seq[String]] =
      {
        var typeParam_h: List[String] = List();
        var typeParam_t: List[String] = List();
        SetVarBindPTP.entPred_h = FolVariableExpression(Variable("entailment_h"));
        SetVarBindPTP.entPred_t = FolVariableExpression(Variable("entailment_t"));

        if (!varBind) { //without variable binding or RTE
          typeParam_h = List("ent_h");
          typeParam_t = List("ent_t");

          SetVarBindPTP.entPred_t = FolApplicationExpression(SetVarBindPTP.entPred_t, FolVariableExpression(Variable("entConst_t")));  
          SetVarBindPTP.entPred_h = FolApplicationExpression(SetVarBindPTP.entPred_h, FolVariableExpression(Variable("entConst_h")));
        } else { //with variable binding.
          val variables: Seq[Variable] = goal.getVariables.toSeq; //find all variables and use them to construct the predicate
          for (v <- variables) {  
            v.name.charAt(0) match {
              case 't' => SetVarBindPTP.entPred_t = FolApplicationExpression(SetVarBindPTP.entPred_t, FolVariableExpression(Variable(v.name)));
              case 'h' => SetVarBindPTP.entPred_h = FolApplicationExpression(SetVarBindPTP.entPred_h, FolVariableExpression(Variable(v.name)));
              case _ => throw new RuntimeException("unsupported type");
            }

            v.name.substring(0, 2) match {
              case "tx" => typeParam_t ::= "indv_t";
              case "te" => typeParam_t ::= "evnt_t";
              case "tp" => typeParam_t ::= "prop_t";
              case "hx" => typeParam_h ::= "indv_h";
              case "he" => typeParam_h ::= "evnt_h";
              case "hp" => typeParam_h ::= "prop_h";
              case _ => throw new RuntimeException("unsupported type");
            }
          }
        }

        typeParam_h = typeParam_h.reverse;
        typeParam_t = typeParam_t.reverse;
        val entailedDec_h = SetVarBindPTP.entPred_h -> typeParam_h;
        val entailedDec_t = SetVarBindPTP.entPred_t -> typeParam_t;

        (Sts.opts.task match {
          case "sts" => Map(entailedDec_h) ++ Map(entailedDec_t);
          case "rte" => Map(entailedDec_h);
        })

      }

    var result: Option[Double] = Some(0.0);

    //first run with variable binding   
    if (Sts.opts.varBind == true && Sts.opts.task == "sts") {
      val entDeclar = getEntDeclar(true);
      result = delegate.prove(constants, declarations ++ entDeclar, evidence, assumptions, goal);
    }
    
    //second run without variable binding. (could be the first if variable binding is disabled or task ir RTE)
    if(result.isEmpty || Sts.opts.varBind == false || Sts.opts.task == "rte")
    {
    	val entDeclar = getEntDeclar(false);
   	    val entConst = Sts.opts.task match {
	      case "sts" => Map("ent_h" -> Set("entConst_h")) ++ Map("ent_t" -> Set("entConst_t"))
	      case "rte" => Map("ent_h" -> Set("entConst_h"))
	    }
    	result = delegate.prove(constants ++ entConst, declarations ++ entDeclar, evidence, assumptions, goal);
    }

    return result;
  }
}
