package utcompling.mlnsemantics.inference

import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.mlnsemantics.vecspace.BowVectorSpace
import utcompling.scalalogic.drt.expression.DrtExpression
import edu.mit.jwi.item.POS
import utcompling.mlnsemantics.wordnet.Wordnet
import utcompling.mlnsemantics.wordnet.WordnetImpl

class InferenceRuleGenerator(vecspace: Map[String, BowVector], wordnet: Wordnet) {

  def generate(from: Set[(String, String)], to: Set[(String, String)]): Iterable[DrtExpression] = {

    //    for (
    //      (f, fPos) <- from;
    //      (t, tPos) <- to
    //    ) yield {
    //
    //      wordnet.synsets(f, fPos)
    //
    //    }

    //
    List()
  }

  def Pos(s: String) =
    s match {
      case "a" | "r" => Set(POS.ADJECTIVE, POS.ADVERB)
      case "n" => Set(POS.NOUN)
      case "v" => Set(POS.ADJECTIVE)
    }

}

object InferenceRuleGenerator {

  def main(args: Array[String]) = {

    val words = Set("baseball", "hockey", "outfield", "puck")
    val vs = BowVectorSpace("resources/nytgiga.lem.1m.vc.f2000.m50.wInf", words)
    val wn = new WordnetImpl
    val irg = new InferenceRuleGenerator(vs, wn)

    val from = Set(("architect", "n"), ("buy", "v"), ("red", "a"), ("car", "n"))
    val to = Set(("person", "n"), ("purchase", "v"), ("vehicle", "n"))

    irg.generate(from, to).foreach(println)

  }

}
