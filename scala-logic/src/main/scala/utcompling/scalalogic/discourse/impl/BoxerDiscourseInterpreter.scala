package utcompling.scalalogic.discourse.impl

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import opennlp.scalabha.util.FileUtils
import utcompling.scalalogic.base.expression.BaseExpression
import scala.collection.mutable.MapBuilder
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.call.Boxer
import utcompling.scalalogic.discourse.candc.call.Candc
import utcompling.scalalogic.discourse.candc.call.impl.BoxerImpl
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPrs


/**
 * An interface to the Boxer software
 * [a href='http://svn.ask.it.usyd.edu.au/trac/candc/wiki/boxer']http://svn.ask.it.usyd.edu.au/trac/candc/wiki/boxer[/a]
 */
class BoxerDiscourseInterpreter[T](
  private val boxerExpressionInterpreter: BoxerExpressionInterpreter[T] = new Boxer2DrtExpressionInterpreter(),
  private val candc: Candc = CandcImpl.findBinary(),
  private val boxer: Boxer = BoxerImpl.findBinary(),
  private val kbest: Int = 1,
  private val verbose: Boolean = false)
  extends DiscourseInterpreter[T] {

  override def batchInterpretMultisentence(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[T]] = {
    val newDiscourseIds = discourseIds.getOrElse((0 until inputs.length).map(_.toString).toList)
    require(inputs.length == newDiscourseIds.length)
/*
    val candcDependencyParseArgs = Map[String, String](
      "--candc-printer" -> "deps",	
      "--candc-parser-kbest" -> kbest.toString(),
      "--candc-int-betas" -> "0.075 0.03 0.01 0.005 0.001")
    val candcDependencyParseOut = this.candc.batchParseMultisentence(inputs, candcDependencyParseArgs.toMap, Some(newDiscourseIds), Some(if (question) "questions" else "boxer"), verbose = verbose)
    println(candcDependencyParseOut)
  */  
    val candcArgs = Map[String, String](
      "--candc-printer" -> "boxer",	
      "--candc-parser-kbest" -> kbest.toString(),
      "--candc-int-betas" -> "0.075 0.03 0.01 0.005 0.001")
    val candcOut = this.candc.batchParseMultisentence(inputs, candcArgs.toMap, Some(newDiscourseIds), Some(if (question) "questions" else "boxer"), verbose = verbose)
    
    val boxerArgs = Map[String, String](
      "--box" -> "false",
      "--semantics" -> "drs",
      "--flat" -> "false",
      "--resolve" -> "true",
      "--elimeq" -> "true",
      "--format" -> "prolog",
      "--instantiate" -> "true")
    //val (boxerOut, scores) = this.boxer.callBoxer(candcOut, boxerArgs.toMap, verbose = verbose)
    val boxerOut = this.boxer.callBoxer(candcOut, boxerArgs.toMap, verbose = verbose)

    val drsDict = this.parseBoxerOutput(boxerOut)//(boxerOut, scores)

    return newDiscourseIds.map(s=> {
      drsDict.find(x => x._1 == s) match {
        case Some((id, exps)) =>  {
          val e = BoxerPrs(exps.toList.asInstanceOf[List[(BoxerExpression, Double)]])
          Some (this.boxerExpressionInterpreter.interpret(e))
        }
        case _ => None 
      }
       
    }).toList
  }

  private def parseBoxerOutput(boxerOut: String/* scores: ListBuffer[String]*/): Map[String, ListBuffer[(T, Double)]] = {
    //val drsDict = new MapBuilder[String, List[T], Map[String, List[T]]](Map[String, List[T]]())
    val drsDict:Map[String, ListBuffer[(T, Double)]] = Map();
    val singleQuotedRe = """^'(.*)'$""".r
    //val scoresItr = scores.iterator;
    val lines = boxerOut.split("\n").iterator
    val IdLineRe = """^id\((\S+),\s*(\d+)\)\.$""".r
    val SemLineRe = """^sem\((\d+),$""".r
    for (line <- lines.map(_.trim)) {
      //println(line)
      line match {
        case IdLineRe(discourseIdWithScore, drsId) =>
          //lines.next.trim match { case SemLineRe(drsId2) => require(drsId == drsId2, "%s != %s".format(drsId, drsId2)) }
          //lines.next.trim match { case l if l.startsWith("[word(") => }
          //lines.next.trim match { case l if l.startsWith("[pos(") => }
          //lines.next.trim match { case l if l.startsWith("[") => }
          lines.next.trim match { case l if l.startsWith("sem(") => }
          val drsInput = lines.next.trim.stripSuffix(").")
          val idWithScoreSplits = discourseIdWithScore.split("-", 2);
          val discourseId = idWithScoreSplits.apply(0)
          val score = idWithScoreSplits.apply(1).replace("'", "").toDouble;
          val cleanDiscourseId = singleQuotedRe.findFirstMatchIn(discourseId).map(_.group(1)).getOrElse(discourseId)
          val parsed = this.parseOutputDrs(drsInput, cleanDiscourseId)
          /*if (!scoresItr.hasNext)
            throw new RuntimeException("number of parsing scores is less than number of parses"); 
          val scoreLine = scoresItr.next;
          println (scoreLine)
          val scoreSplits = scoreLine.split(',');
          val score = scoreSplits.apply(0).substring(11).toDouble;
          val scoreId = scoreSplits.apply(1).substring(5, scoreSplits.apply(1).length()-2)
          if (scoreId != cleanDiscourseId)
            throw new RuntimeException("score's ID does not match Boxer's ID");
           */
          drsDict.find(x => x._1 == cleanDiscourseId) match {
            case Some((id, l)) => l += ((this.boxerExpressionInterpreter.interpret(parsed), score));
            case _ => drsDict += cleanDiscourseId -> ListBuffer((this.boxerExpressionInterpreter.interpret(parsed), score)) 
          }
          
        case _ =>
      }
    }
    return drsDict
  }

  private def parseOutputDrs(drsString: String, discourseId: String): BoxerExpression = {
    return new BoxerExpressionParser(discourseId).parse(drsString)
  }

}

object BoxerDiscourseInterpreter {

  def main(args: Array[String]) {
    
   
    val bdi = new BoxerDiscourseInterpreter (  );
    
    /*bdi.parseBoxerOutput ( """%%% Two men are fistfighting in a ring .  
id('2',3).
sem(3,
    [word(3001, 'Two'), word(3002, men), word(3003, are), word(3004, fistfighting), word(3005, in), word(3006, a), word(3007, ring), word(3008, '.')],
    [pos(3001, 'CD'), pos(3002, 'NNS'), pos(3003, 'VBP'), pos(3004, 'VBG'), pos(3005, 'IN'), pos(3006, 'DT'), pos(3007, 'NN'), pos(3008, '.')],
    [],
    drs([[]:x0, []:e1, [3006]:x2], [[3001]:card(x0, 2, eq), [3002]:pred(x0, man, n, 0), [3004]:pred(e1, fistfight, v, 0), []:rel(e1, x0, agent, 0), [3007]:pred(x2, ring, n, 0), [3005]:rel(e1, x2, in, 0)]) ).
""");
    */
    /*bdi.parseBoxerOutput ( """%%% But US stock prices fell only 5.2 % between May 9 and May 24 . 
id('0',1).
sem(1,
    [word(1001, 'But'), word(1002, 'US'), word(1003, stock), word(1004, prices), word(1005, fell), word(1006, only), word(1007, '5.2'), word(1008, '%'), word(1009, between), word(1010, 'May'), word(1011, '9'), word(1012, and), word(1013, 'May'), word(1014, '24'), word(1015, '.')],
    [pos(1001, 'CC'), pos(1002, 'NNP'), pos(1003, 'NN'), pos(1004, 'NNS'), pos(1005, 'VBD'), pos(1006, 'RB'), pos(1007, 'CD'), pos(1008, 'NN'), pos(1009, 'IN'), pos(1010, 'NNP'), pos(1011, 'CD'), pos(1012, 'CC'), pos(1013, 'NNP'), pos(1014, 'CD'), pos(1015, '.')],
    [ne(1002, 'I-LOC'), ne(1007, 'I-PCT'), ne(1008, 'I-PCT'), ne(1009, 'I-PCT'), ne(1010, 'I-DAT'), ne(1011, 'I-DAT'), ne(1013, 'I-DAT'), ne(1014, 'I-DAT')],
    drs([[]:x0, []:x1, []:e2, []:x3, []:x4, []:x5, []:x6], [[1002]:named(x0, us, loc, 0), [1003]:pred(x1, stock, n, 0), []:rel(x0, x1, nn, 0), [1004]:pred(x0, price, n, 0), [1005]:pred(e2, fall, v, 0), []:rel(e2, x0, agent, 0), [1008]:pred(x3, '%', n, 0), []:rel(e2, x3, rel, 0), [1007]:card(x4, 5.2, eq), []:rel(e2, x4, rel, 0), [1006]:pred(e2, only, a, 0), [1010, 1011]:timex(x5, date([]: +, []:'XXXX', [1010]:'05', [1011]:'09')), [1009]:rel(e2, x5, between, 0), [1001]:pred(e2, but, a, 0), [1013, 1014]:timex(x6, date([]: +, []:'XXXX', [1013]:'05', [1014]:'24')), [1009]:rel(e2, x6, between, 0), [1001]:pred(e2, but, a, 0)]) ).
""");*/
    
/*    bdi.parseBoxerOutput ( """%%% On my own behalf and on behalf of my colleagues in the Committee on Fisheries , I would ask you , Madam President , to send Parliament ' s condolences to the families of the victims and to the local authorities in both Brittany and in Marín , Galicia , from where the majority of the victims came . 
id('0',1).
sem(1,
    [word(1001, 'On'), word(1002, my), word(1003, own), word(1004, behalf), word(1005, and), word(1006, on), word(1007, behalf), word(1008, of), word(1009, my), word(1010, colleagues), word(1011, in), word(1012, the), word(1013, 'Committee'), word(1014, on), word(1015, 'Fisheries'), word(1016, ','), word(1017, 'I'), word(1018, would), word(1019, ask), word(1020, you), word(1021, ','), word(1022, 'Madam'), word(1023, 'President'), word(1024, ','), word(1025, to), word(1026, send), word(1027, 'Parliament'), word(1028, '\''), word(1029, s), word(1030, condolences), word(1031, to), word(1032, the), word(1033, families), word(1034, of), word(1035, the), word(1036, victims), word(1037, and), word(1038, to), word(1039, the), word(1040, local), word(1041, authorities), word(1042, in), word(1043, both), word(1044, 'Brittany'), word(1045, and), word(1046, in), word(1047, 'Marín'), word(1048, ','), word(1049, 'Galicia'), word(1050, ','), word(1051, from), word(1052, where), word(1053, the), word(1054, majority), word(1055, of), word(1056, the), word(1057, victims), word(1058, came), word(1059, '.')],
    [pos(1001, 'IN'), pos(1002, 'PRP$'), pos(1003, 'JJ'), pos(1004, 'NN'), pos(1005, 'CC'), pos(1006, 'IN'), pos(1007, 'NN'), pos(1008, 'IN'), pos(1009, 'PRP$'), pos(1010, 'NNS'), pos(1011, 'IN'), pos(1012, 'DT'), pos(1013, 'NNP'), pos(1014, 'IN'), pos(1015, 'NNPS'), pos(1016, ','), pos(1017, 'PRP'), pos(1018, 'MD'), pos(1019, 'VB'), pos(1020, 'PRP'), pos(1021, ','), pos(1022, 'NNP'), pos(1023, 'NNP'), pos(1024, ','), pos(1025, 'TO'), pos(1026, 'VB'), pos(1027, 'NNP'), pos(1028, 'POS'), pos(1029, 'VBZ'), pos(1030, 'NNS'), pos(1031, 'TO'), pos(1032, 'DT'), pos(1033, 'NNS'), pos(1034, 'IN'), pos(1035, 'DT'), pos(1036, 'NNS'), pos(1037, 'CC'), pos(1038, 'TO'), pos(1039, 'DT'), pos(1040, 'JJ'), pos(1041, 'NNS'), pos(1042, 'IN'), pos(1043, 'DT'), pos(1044, 'NNP'), pos(1045, 'CC'), pos(1046, 'IN'), pos(1047, 'NNP'), pos(1048, ','), pos(1049, 'NNP'), pos(1050, ','), pos(1051, 'IN'), pos(1052, 'WRB'), pos(1053, 'DT'), pos(1054, 'NN'), pos(1055, 'IN'), pos(1056, 'DT'), pos(1057, 'NNS'), pos(1058, 'VBD'), pos(1059, '.')],
    [ne(1013, 'I-ORG'), ne(1015, 'I-ORG'), ne(1027, 'I-ORG'), ne(1044, 'I-LOC'), ne(1047, 'I-LOC'), ne(1049, 'I-LOC')],
    alfa(top, drs([[]:x0, []:x1, []:x2, [1053]:x3, [1056]:x4, []:x5, []:x6, []:x7, [1032]:x8, [1039]:x9, [1043]:x10, []:x11, []:x12, []:x13, []:x14, []:x15, []:x16, [1012]:x17, []:x18], [[1049]:named(x12, galicia, loc, 0), [1047]:named(x11, marín, loc, 0), [1044]:named(x10, brittany, loc, 0), [1041]:pred(x9, authority, n, 0), [1040]:pred(x9, local, a, 0), [1057]:pred(x4, victim, n, 0), [1033]:pred(x8, family, n, 0), [1027]:named(x7, parliament, org, 0), [1054]:pred(x3, majority, n, 0), [1022, 1023]:named(x2, madam_president, nam, 0), [1017]:pred(x0, person, n, 1), [1020]:pred(x1, person, n, 1), [1021]:rel(x1, x2, rel, 2), []:pred(x5, person, n, 1), [1002]:rel(x6, x5, of, 0), [1003]:pred(x6, own, a, 0), [1004]:pred(x6, behalf, n, 0), [1048]:rel(x11, x12, rel, 2), [1017]:pred(x13, person, n, 1), [1020]:pred(x14, person, n, 1), [1021]:rel(x14, x2, rel, 2), []:pred(x15, person, n, 1), [1009]:rel(x16, x15, of, 0), [1010]:pred(x16, colleague, n, 0), [1013]:named(x17, committee, org, 0), [1015]:named(x18, fisheries, org, 0), [1048]:rel(x11, x12, rel, 2)]), drs([[]:e19, []:p20, []:x21, []:x22, []:e23, []:x24, []:e25, []:x26, []:e27, []:e28, []:e29, []:p30, []:x31, []:x32, []:x33, []:e34, []:x35, []:e36, []:x37, []:e38, []:e39], [[1019]:pred(e19, ask, v, 0), []:rel(e19, x1, recipient, 0), []:rel(e19, x0, agent, 0), []:rel(e19, p20, theme, 0), []:prop(p20, drs([], [[]:whq([loc:any], drs([[]:x40], [[1052]:pred(x40, location, n, 1)]), x40, drs([[]:e41], [[1055]:rel(x3, x4, of, 0), [1058]:pred(e41, come, v, 0), []:rel(e41, x3, agent, 0), []:rel(e41, x40, loc_rel, 0)]))])), [1001]:rel(e19, x6, on, 0), [1028]:pred(x21, '\'', n, 0), [1030]:pred(x22, condolence, n, 0), [1034]:rel(x8, x4, of, 0), [1031]:rel(x22, x8, to, 0), [1029]:pred(e23, s, v, 0), []:rel(e23, x21, agent, 0), []:rel(e23, x22, patient, 0), [1051]:rel(e23, x7, from, 0), [1030]:pred(x24, condolence, n, 0), [1042]:rel(x9, x10, in, 0), [1038]:rel(x24, x9, to, 0), [1029]:pred(e25, s, v, 0), []:rel(e25, x21, agent, 0), []:rel(e25, x24, patient, 0), [1051]:rel(e25, x7, from, 0), [1030]:pred(x26, condolence, n, 0), [1046]:rel(x26, x11, in, 0), [1029]:pred(e27, s, v, 0), []:rel(e27, x21, agent, 0), []:rel(e27, x26, patient, 0), [1051]:rel(e27, x7, from, 0), [1026]:pred(e28, send, v, 0), []:rel(e28, x0, agent, 0), []:rel(e28, x7, patient, 0), [1019]:pred(e29, ask, v, 0), []:rel(e29, x14, recipient, 0), []:rel(e29, x13, agent, 0), []:rel(e29, p30, theme, 0), []:prop(p30, drs([], [[]:whq([loc:any], drs([[]:x42], [[1052]:pred(x42, location, n, 1)]), x42, drs([[]:e43], [[1055]:rel(x3, x4, of, 0), [1058]:pred(e43, come, v, 0), []:rel(e43, x3, agent, 0), []:rel(e43, x42, loc_rel, 0)]))])), [1007]:pred(x31, behalf, n, 0), [1014]:rel(x17, x18, on, 0), [1011]:rel(x16, x17, in, 0), [1008]:rel(x31, x16, of, 0), [1006]:rel(e29, x31, on, 0), [1028]:pred(x32, '\'', n, 0), [1030]:pred(x33, condolence, n, 0), [1034]:rel(x8, x4, of, 0), [1031]:rel(x33, x8, to, 0), [1029]:pred(e34, s, v, 0), []:rel(e34, x32, agent, 0), []:rel(e34, x33, patient, 0), [1051]:rel(e34, x7, from, 0), [1030]:pred(x35, condolence, n, 0), [1042]:rel(x9, x10, in, 0), [1038]:rel(x35, x9, to, 0), [1029]:pred(e36, s, v, 0), []:rel(e36, x32, agent, 0), []:rel(e36, x35, patient, 0), [1051]:rel(e36, x7, from, 0), [1030]:pred(x37, condolence, n, 0), [1046]:rel(x37, x11, in, 0), [1029]:pred(e38, s, v, 0), []:rel(e38, x32, agent, 0), []:rel(e38, x37, patient, 0), [1051]:rel(e38, x7, from, 0), [1026]:pred(e39, send, v, 0), []:rel(e39, x13, agent, 0), []:rel(e39, x7, patient, 0)])) ).
""");
*/
    bdi.parseBoxerOutput ( """id(1,1).
sem(1,[1001:[tok:'A', pos:'DT', lemma:a, namex:'O'], 1002:[tok:person, pos:'NN', lemma:person, namex:'O'], 1003:[tok: (is), pos:'VBZ', lemma:be, namex:'O'], 1004:[tok:throwing, pos:'VBG', lemma:throw, namex:'O'], 1005:[tok:a, pos:'DT', lemma:a, namex:'O'], 1006:[tok:cat, pos:'NN', lemma:cat, namex:'O'], 1007:[tok:on, pos:'IN', lemma:on, namex:'O'], 1008:[tok:to, pos:'TO', lemma:to, namex:'O'], 1009:[tok:the, pos:'DT', lemma:the, namex:'O'], 1010:[tok:ceiling, pos:'NN', lemma:ceiling, namex:'O'], 1011:[tok:'.', pos:'.', lemma:'.', namex:'O']],
alfa(top, drs([[1009]:x0], [[1010]:pred(x0, ceiling, n, 0)]), drs([[]:x1, []:x2, [1005]:x3, [1001]:x4, []:x5], [[]:rel(x1, x4, patient, 0), []:rel(x1, x5, agent, 0), _G4379:pred(x1, event, v, 0), []:rel(x2, x3, patient, 0), []:rel(x2, x4, agent, 0), [1004]:pred(x2, throw, v, 0), [1006]:pred(x3, cat, n, 0), [1002]:pred(x4, person, n, 0), []:pred(x5, thing, n, 12), [1008]:rel(x1, x0, to, 0), [1007]:pred(x1, on, a, 0)]))).
""");

  }
}
