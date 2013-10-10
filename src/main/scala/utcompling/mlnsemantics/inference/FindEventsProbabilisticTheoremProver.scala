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
import utcompling.mlnsemantics.run.Sts

class FindEventsProbabilisticTheoremProver(
  delegate: ProbabilisticTheoremProver[BoxerExpression])
  extends ProbabilisticTheoremProver[BoxerExpression] {

  private val LOG = LogFactory.getLog(classOf[FindEventsProbabilisticTheoremProver])

  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[BoxerExpression, Seq[String]],
    evidence: List[BoxerExpression],
    assumptions: List[WeightedExpression[BoxerExpression]],
    goal: BoxerExpression): Option[Double] = {

    var newAssumption = assumptions.head.expression;
    var newGoal = goal;

    var newDeclarationsDetailed : Map[BoxerExpression/*representative exp*/, Seq[(String/*x,e,p,d*/, Set/*vars confirming this type*/[(String/*t,h*/, String/*varname*/)])]] = Map();
    
    var conflicts:Set[(String/*t,h*/, String/*varname*/)] = Set();
    
    var eventVars:Set[(String, String)] = Set();
    var propVars:Set[(String, String)] = Set();
    var TOrH = "t";
    if (Sts.opts.withEventProp){
	    eventVars = findEventVar(newAssumption).toSet;
	    propVars = findPropVar(newAssumption).toSet;
	    eventVars = eventVars ++ findEventVar(newGoal).toSet;
	    propVars = propVars ++ findPropVar(newGoal).toSet;
    }
    
    findDeclarations(newAssumption);//fill in newDeclarationsDetailed and conflicts
    findDeclarations(newGoal);//fill in newDeclarationsDetailed and conflicts
    
    propagateConflicts();

    newAssumption = convertToEvntPropVar(newAssumption);//use conflicts, eventVars and propVars to rename variables
    TOrH = "h";
    newGoal = convertToEvntPropVar(newGoal); //use conflicts, eventVars and propVars to rename variables
    

    object AllDone extends Exception { }
    def propagateConflicts() = 
    {
      var changed = true;
      while (changed)
      {
    	  changed = false;
    	  try
    	  {
	    	  newDeclarationsDetailed.foreach(declaration => 
	    	  {
	    		  var types = declaration._2
	    	      for(i<-0 to types.length - 1)
	    		  {
	    			  	val conf = types.get(i)
		    		    if (conf._1 == "e" || conf._1 == "p") 
			    		{
			    			  if (!(conflicts & conf._2).isEmpty)
			    			  {
			    				  conflicts = conflicts  ++ conf._2;
			    				  changed = true;
			    				  types = types.updated(i, ("c", conf._2))
			    				  newDeclarationsDetailed += (declaration._1 -> types);
			    				  //change entry in declarations
			    				  throw AllDone;
			    			  }
			    		}
	    		  }
	    	  })
    	  }catch {case AllDone =>} //simulating break;
      }
    }
    
    def addDeclaration(exp: BoxerExpression, types: Seq[BoxerVariable], discId: String) = {
    	try{
    		var existingTypes = newDeclarationsDetailed.apply(exp);
    		assert(existingTypes.length == types.length)
    		for(i<-0 to existingTypes.length - 1)
    		{
    		  var changedType =	if (eventVars.contains((discId, types.get(i).name))) "e"
    			  				else if (propVars.contains((discId, types.get(i).name))) "p"
    			  				else "x"
    		  
    		  val existingType = existingTypes.get(i)._1
    		  var conformings = existingTypes.get(i)._2

    		  if (existingType != changedType)
    		  {
    			  conflicts = conflicts  ++ conformings
    			  conflicts += ((discId, types.get(i).name));
    			  changedType = "c";
    			  //existingTypes = existingTypes.updated(i, ("c", Set[(String, String)]()))
    		  }
    		  conformings += ((discId, types.get(i).name));
    		  existingTypes = existingTypes.updated(i, (changedType, conformings))
    		    
    		}
    		newDeclarationsDetailed += (exp -> existingTypes);
    	}catch {
    	  case e:NoSuchElementException =>
    	    newDeclarationsDetailed += (exp -> (types.map(t=>{
      		  val changedType =	if (eventVars.contains((discId, t.name))) "e"
	  				else if (propVars.contains((discId, t.name))) "p"
	  				else "x"    	      
    	      (changedType, Set((discId, t.name)))
    	    })))
    	}
    }
    
   def findDeclarations(e: BoxerExpression): Int = {
      e match {
        case BoxerPred(discId, indices, variable, name, pos, sense) => 
          addDeclaration(BoxerPred("h", List(), BoxerVariable("x"), name, pos, 0), Seq(variable), discId)

        case BoxerNamed(discId, indices, variable, name, typ, sense) =>
          addDeclaration(BoxerNamed("h", List(), BoxerVariable("x"), name, "", 0), Seq(variable), discId)
          
        case BoxerRel(discId, indices, event, variable, name, sense) =>
          addDeclaration(BoxerRel("h", List(), BoxerVariable("x"), BoxerVariable("x"), name, 0), Seq(event, variable), discId)
          
        case BoxerCard(discId, indices, variable, num, typ) =>
          addDeclaration(BoxerCard("h", List(), BoxerVariable("x"), num, ""), Seq(variable), discId)
          
        case BoxerTimex(discId, indices, variable, timeExp) =>
          addDeclaration(BoxerTimex("h", List(), BoxerVariable("x"), BoxerVariable("x")), Seq(variable), discId)
        
        case _ => e.visit(findDeclarations, (x: List[Int]) => 0, 0)
      }
      return 0;
    }

    def convertToEvntPropVar(e: BoxerExpression): BoxerExpression = {
      e match {
        case BoxerImp(discId, indices, first, second) =>{
          val changedFirst = convertToEvntPropVar(first)
          val changedSecond = convertToEvntPropVar(second)
          Sts.opts.keepUniv match { 
   	  		case true => BoxerImp(discId, indices, changedFirst, changedSecond) 
   	  		case false => //This is just a hack to remove all Univ quantifiers
   	  		  			//Imp is replaced with Prop because Prop will be removed in UnnecessarySubboxRemovingBoxerExpressionInterpreter
   	  		  BoxerProp(discId, indices, BoxerVariable("v"), BoxerDrs(changedFirst.refs ++ changedSecond.refs, changedFirst.conds ++ changedSecond.conds)) 
   	  		}
   		  }  
        case BoxerVariable(name) => {
          if (conflicts.contains((TOrH, name)))
            BoxerVariable("x" + name.substring(1))
          else if (eventVars.contains((TOrH, name)))
            BoxerVariable("e" + name.substring(1))
          else if (propVars.contains((TOrH, name)))
            BoxerVariable("p" + name.substring(1))
          else
            e //also x
        }
        case _ => e.visitConstruct(convertToEvntPropVar)
      }
    }

    val newAssumptions = List(HardWeightedExpression(newAssumption))
    var newConstants =
      Map(
        "indv_h" -> Set("default_indv_h_variable"),
        "evnt_h" -> Set("default_evnt_h_variable"),
        "prop_h" -> Set("default_prop_h_variable")) ;
    
    if (Sts.opts.task == "sts")
      newConstants = newConstants ++ Map(
        "indv_t" -> Set("default_indv_t_variable"),
        "evnt_t" -> Set("default_evnt_t_variable"),
        "prop_t" -> Set("default_prop_t_variable")) ;
    
    val newDeclarations = newDeclarationsDetailed.flatMap(d => {
      List((d._1/*expression*/, /*args types*/d._2.map(t=>
	      t._1 match
	      {
			case "x"=>"indv_h";
			case "e"=>"evnt_h";
			case "p"=>"prop_h";
			case "c"=>"indv_h";//conflict
	      }))) ++
	     (if (Sts.opts.task == "sts")
		      List((d._1/*expression*/ match {
		        case BoxerPred(discId, indices, variable, name, pos, sense) => BoxerPred("t", indices, variable, name, pos, sense) 
		        case BoxerNamed(discId, indices, variable, name, typ, sense) => BoxerNamed("t", indices, variable, name, typ, sense)
		        case BoxerRel(discId, indices, event, variable, name, sense) => BoxerRel("t", indices, event, variable, name, sense)
		        case BoxerCard(discId, indices, variable, num, typ) => BoxerCard("t", indices, variable, num, typ)
		        case BoxerTimex(discId, indices, variable, timeExp) => BoxerTimex("t", indices, variable, timeExp)
		  	  }, /*args types*/d._2.map(t=>
		      t._1 match
		      {
				case "x"=>"indv_t";
				case "e"=>"evnt_t";
				case "p"=>"prop_t";
				case "c"=>"indv_t";//conflict
		      })))
		  else
		    List[(BoxerExpression, Seq[String])]()
		  )
      })
      
    delegate.prove(newConstants, newDeclarations, evidence, newAssumptions, newGoal)
  }

  private def findEventVar(e: BoxerExpression): List[(String, String)] = {
    e match {
      case BoxerRel(discId, indices, event, variable, name, sense) => {
        if (name == "agent" || name == "patient")
          return List((discId, event.name));
        else
          return List();
      }
      case _ => {
        e.visit(findEventVar, (parts: List[List[(String, String)]]) => {
          var compined = List[(String, String)]();
          for (p <- parts)
            compined = compined ++ p;
          return compined.toSet.toList
        }, List())
      }
    }
  }

  private def findPropVar(e: BoxerExpression): List[(String, String)] = {
    e match {
      case BoxerProp(discId, indices, variable, drs) =>
        return List((discId, variable.name)) ++ findPropVar(drs)
      case _ => {
        e.visit(findPropVar, (parts: List[List[(String, String)]]) => {
          var compined = List[(String, String)]();
          for (p <- parts)
            compined = compined ++ p;
          return compined.toSet.toList
        }, List())
      }
    }
  }

}
