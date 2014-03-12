package utcompling.mlnsemantics.rules

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.FileUtils._
import org.apache.commons.logging.LogFactory
import utcompling.mlnsemantics.datagen.SimpleTokenizer
import utcompling.mlnsemantics.run.Sts
import scala.Array.canBuildFrom
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.mlnsemantics.vecspace.{BowVector, BowVectorSpace}
import utcompling.mlnsemantics.inference.RuleWeighter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerIndex

class OnTheFlyRules extends Rules {

  private val LOG = LogFactory.getLog(classOf[OnTheFlyRules])

  def getRules(text: BoxerExpression, hypothesis: BoxerExpression, ruleWeighter: RuleWeighter,
    vecspaceFactory: ((String => Boolean) => BowVectorSpace)): List[(BoxerDrs, BoxerDrs, Double)] =
    {
      val assumPredsAndContexts = getAllPredsAndContexts(text)
      val goalPredsAndContexts = getAllPredsAndContexts(hypothesis)
      val allPredsAndContexts = List.concat(assumPredsAndContexts, goalPredsAndContexts);
      val vectorspace = vecspaceFactory(allPredsAndContexts.flatMap {
        case (pred, context) => {
          val l = List(pred.name + (/*Sts.opts.vectorspaceFormatWithPOS*/ false match {
            case true => "-" + pred.pos;
            case false => "";
          })) ++ context
          l
        }
      }.toSet)

      val assumRel = text.getRelations().flatMap(r=>{
        r match { //do not generate inference rules connected with a "theme" relation
          case BoxerRel(discId, indices, event, variable, name, sense) if (name.contains("theme")) => None;
          case _ => Some(r)
        } 
      });
      val goalRel = hypothesis.getRelations().flatMap(r=>{
        r match {
          case BoxerRel(discId, indices, event, variable, name, sense) if (name.contains("theme")) => None;
          case _ => Some(r)
        } 
      });

      // Use this map to check variable types in inference rules
      var predTypeMap = Map[String, String]()
      for (assumPredsAndContext <- assumPredsAndContexts)
        predTypeMap += ((assumPredsAndContext._1.name) -> variableType(assumPredsAndContext._1))
      for (goalPredsAndContext <- goalPredsAndContexts)
        predTypeMap += ((goalPredsAndContext._1.name) -> variableType(goalPredsAndContext._1))

      //create rules
      var rules = List[(BoxerDrs, BoxerDrs, Double)]() ;
   if (Sts.opts.inferenceRulesLevel > 0){ //if > 0, add lexical and phrasal distrbutional rules
    
      //findRelPred handels if inferenceRulesLevel is 1 or 2
      val assumRelPred = findRelPred(assumPredsAndContexts, assumRel);
      val goalRelPred = findRelPred(goalPredsAndContexts, goalRel);
        
      for (goalEntry <- goalRelPred )
      {
          for (assumEntry <- assumRelPred )
          {
            //DO not add rules if the word is the same??? Why?
            //Words should have the same POS and same entity type (individual or event)
            
           if (//assumPred._1.pos == goalPred._1.pos && 
              assumEntry._3 != goalEntry._3 )
              //no need to check for the variable anymore before all of them are INDV now. 
              //assumPred._1.variable.name.charAt(0) == goalPred._1.variable.name.charAt(0))
            {
            val rw = ruleWeighter.weightForRules(assumEntry._3, assumEntry._2, Seq((goalEntry._3 , goalEntry._2)).toMap, vectorspace);
            
            var assumEntryExp = assumEntry._1
            var goalEntryExp = goalEntry._1
            
            val assumEntryPreds = assumEntryExp.getPredicates()
            val goalEntryPreds = goalEntryExp.getPredicates()
            
            //If LHS and RHS are both single predicates, use the same variable name for both of them
            if(assumEntryPreds.length == 1 && goalEntryPreds.length == 1)
            {
              val assumPred = assumEntryPreds.head match {
                case BoxerPred(discId, indices, variable, name, pos, sense)=>
                  BoxerPred(discId, indices, BoxerVariable("x0"), name, pos, sense)
              }
              val goalPred = goalEntryPreds.head match {
                case BoxerPred(discId, indices, variable, name, pos, sense)=>
                  BoxerPred(discId, indices, BoxerVariable("x0"), name, pos, sense)
              }
              assumEntryExp = BoxerDrs(List(List() -> BoxerVariable("x0")), List(assumPred));
              goalEntryExp = BoxerDrs(List(List() -> BoxerVariable("x0")), List(goalPred));
            }
            
            val assumeVarTypeMap = assumEntryExp.getPredicates().map(pred => (pred.variable.name, predTypeMap(pred.name))).toMap
            val goalVarTypeMap = goalEntryExp.getPredicates().map(pred => (pred.variable.name, predTypeMap(pred.name))).toMap
          
              if (rw.head._2.get > 0 && checkCompatibleType(assumeVarTypeMap, goalVarTypeMap))
                rules = rules ++ List((assumEntryExp, goalEntryExp, rw.head._2.get))
              
            }
          }
       } 
  }
  return rules;
    }
  
  private def indicesToIndex(l:List[BoxerIndex]):Int = 
  {
    if( l.length != 1)
      0 // index is not unique or does not exist 
    else
      l.head.wordIndex+1 //index starts from 1 
  }
  
  private def getAllPredsAndContexts(e: BoxerExpression): Seq[(BoxerPred, Seq[String])] =
    {
      val preds = e.getPredicates();
      val predsAndContexts = preds.zipWithIndex.map { case (p, i) => p -> (preds.take(i) ++ preds.drop(i + 1)) }
      val newPredsAndContexts = predsAndContexts.mapVals(_.map(p => (p.name + (Sts.opts.vectorspaceFormatWithPOS match {
        case true => "-" + p.pos + "-" + indicesToIndex(p.indices);
        case false => "";
      }))));
      val newPredsAndContextsSplit = newPredsAndContexts.mapVals(l => {
        var flatL: List[String] = List();
        l.foreach(e => {
          flatL = flatL ++ List(e);
        })
        flatL
      })
      return newPredsAndContextsSplit; //predsAndContexts.mapVals(_.map(p => stripNot(p.name))) ;
    }
  
  private def checkCompatibleType(
    assumeVarTypeMap: Map[String, String],
    goalVarTypeMap: Map[String, String]): Boolean =
    {
      assumeVarTypeMap.foreach(e =>
        {
          try {
            if (e._2 != goalVarTypeMap.get(e._1).get)
              return false;
          } catch {
            case e: NoSuchElementException =>
          }
        })
      goalVarTypeMap.foreach(e =>
        {
          try {
            if (e._2 != assumeVarTypeMap.get(e._1).get)
              return false;
          } catch {
            case e: NoSuchElementException =>
          }
        })
      return true;
    }

  private def findRelPred(preds: Iterable[(BoxerPred, Iterable[String])], rel: Iterable[BoxerRel]): Set[(BoxerDrs, Iterable[String], String)] = {
    var mapPredVar = new HashMap[String, scala.collection.mutable.Set[(BoxerPred, Iterable[String])]] with MultiMap[String, (BoxerPred, Iterable[String])];
    (preds.map(row => row._1.variable.name -> row)).foreach(x => {
      mapPredVar.addBinding(x._1, x._2);
    })

    var notUsedPred = preds.toMap;

    if (Sts.opts.inferenceRulesLevel == 1) //no phrases
      mapPredVar.clear;

    val sameVarList: List[(BoxerDrs, List[String], String)] =
      mapPredVar.flatMap(predPairList =>
        {
          if (predPairList._2.size > 1) {
            //string for vector building  
            val w = predPairList._2.foldLeft("")((words, predPair) => {
              val word = Sts.opts.vectorspaceFormatWithPOS match {
                case true => predPair._1.name + "-" + predPair._1.pos + "-" + indicesToIndex(predPair._1.indices);
                case false => predPair._1.name;
              }
              if (words == "")
                word
              else
                (words + " " + word) //separate words of a phrase with a space
            })

            //println("//PHRASE(pp): " + w)

            //context
            var context = predPairList._2.flatMap(predPair => predPair._2)

            predPairList._2.foreach(predPair => (context = context - predPair._1.name))

            //predicates list
            var cond = predPairList._2.map(predPair =>
              BoxerPred(predPair._1.discId, predPair._1.indices, BoxerVariable("x1"), predPair._1.name, predPair._1.pos, predPair._1.sense)).toList
            val vars = List(List() -> BoxerVariable("x0")) ++ List(List() -> BoxerVariable("x1"));
            val exp = BoxerDrs(vars, cond);

            Some(exp, context.toList, w)
          } else None
        }).toList

    //phrases of the form PredRelPred
    rel.flatMap(r => {
      if (mapPredVar.contains(r.event.name) && mapPredVar.contains(r.variable.name)) {
        val arg1Set = mapPredVar(r.event.name)
        val arg2Set = mapPredVar(r.variable.name)

        var phrasesList: List[(BoxerDrs, List[String], String)] = List();

        arg1Set.map(arg1 =>
          arg2Set.map(arg2 => {
            notUsedPred = notUsedPred - arg1._1;
            notUsedPred = notUsedPred - arg2._1;

            val (varName1, varName2) = if (arg2._1.pos == "v") ("x1", "x0")
            else ("x0", "x1")

            val arg1Changed = BoxerPred(arg1._1.discId, arg1._1.indices, BoxerVariable(varName1), arg1._1.name, arg1._1.pos, arg1._1.sense)
            val arg2Changed = BoxerPred(arg2._1.discId, arg2._1.indices, BoxerVariable(varName2), arg2._1.name, arg2._1.pos, arg2._1.sense)
            val rChanged = BoxerRel(r.discId, r.indices, BoxerVariable(varName1), BoxerVariable(varName2), r.name, r.sense)

            //println("//PHRASE(prp): " + arg1._1.name+"-"+arg1._1.pos + " " + r.name + " " + arg2._1.name+"-"+arg2._1.pos)

            val context = (arg1._2 ++ arg2._2).toList.diff(List(arg1._1.name)).diff(List(arg2._1.name)).toSet;

            var words = Sts.opts.vectorspaceFormatWithPOS match {
              //use space to split between words of a phrase
              case true => arg1._1.name + "-" + arg1._1.pos + "-" + indicesToIndex(arg1._1.indices) + " " + 
                      arg2._1.name + "-" + arg2._1.pos + "-" + indicesToIndex(arg2._1.indices) + " " +
                      r.name  + "-" + "rel" + "-" +  indicesToIndex(r.indices);                      
              case false => arg1._1.name + " " + arg2._1.name;
            }

            val vars = List(List() -> BoxerVariable("x0")) ++ List(List() -> BoxerVariable("x1"));
            val cond = List(arg1Changed) ++ List(arg2Changed) ++ List(rChanged);
            val exp = BoxerDrs(vars, cond);
            phrasesList = phrasesList ++ List((exp, context.toList, words))
          }))
        phrasesList;
      } else None;
    }).toSet ++
      //phrases of the form PredPred
      sameVarList ++
      //lexical predicates
      (if (Sts.opts.duplicatePhraselAndLexicalRule) preds else notUsedPred).map(p => (
        BoxerDrs(List(List() -> BoxerVariable("x0")) ++ List(List() -> BoxerVariable("x1")), List(BoxerPred(p._1.discId, p._1.indices, BoxerVariable(if (p._1.pos == "v") "x0" else "x1"), p._1.name, p._1.pos, p._1.sense))),
        p._2,
        Sts.opts.vectorspaceFormatWithPOS match { case true => p._1.name + "-" + p._1.pos + "-" + indicesToIndex(p._1.indices); case false => p._1.name; }))
  }
  val VariableRe = """^([a-z])\d*$""".r
  private def variableType(pred: BoxerPred): String =
    pred.variable.name match {
      case VariableRe("p") => "p"
      case VariableRe("e") => "e"
      case _ => "x"
    }
}

object OnTheFlyRules {

}
