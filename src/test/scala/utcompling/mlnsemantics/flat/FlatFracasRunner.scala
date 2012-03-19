package utcompling.mlnsemantics.flat

import utcompling.scalalogic.discourse.impl.BoxerDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.impl.CandcDiscourseParser
import utcompling.mlnsemantics.modal.VisualizingModalTheoremProverDecorator
import utcompling.scalalogic.discourse.candc.call.impl.BoxerImpl
import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.mlnsemantics.modal.ModalTheoremProver
import utcompling.mlnsemantics.polarity.PolarityLexicon
import utcompling.mlnsemantics.data.FracasReader
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.mlnsemantics.data.RtePair
import utcompling.scalalogic.util.StringUtils._
import utcompling.mlnsemantics.data.DataReader
import utcompling.mlnsemantics.data.ExampleChoosingRtePairReaderDecorator
import utcompling.scalalogic.util.FileUtils
import org.junit.Test

class FracasTestRunner {

  @Test
  def test() {

    val r = FracasReader.fromFile()

    //run(r)

    run(new ExampleChoosingRtePairReaderDecorator(Set("fracas-001"), r))

  }

  def run(reader: DataReader[RtePair]) {

    val binDir = Some(FileUtils.pathjoin(System.getenv("HOME"), "bin/candc/bin"))
    val candc = CandcImpl.findBinary(binDir)
    val boxer = BoxerImpl.findBinary(binDir)
    val bei = new MergingBoxerExpressionInterpreterDecorator()
    val bdi = new BoxerDiscourseInterpreter(bei, candc, boxer)
    val cdp = new CandcDiscourseParser(candc)
    val pl = PolarityLexicon.fromFile("resources/polarity-lexicon/polarity_lexicon_expanded.txt")
    val mdi = new ModalDiscourseInterpreter(bdi, cdp, pl)
    def tp = new Prover9TheoremProver(FileUtils.pathjoin(System.getenv("HOME"), "bin/LADR-2009-11A/bin/prover9"), 5, false)
    def mtp = new ModalTheoremProver(tp)
    def vmtp = new VisualizingModalTheoremProverDecorator(mtp)

    var totalCount = 0
    var parsedCount = 0
    var correctCount = 0
    for (RtePair(id, premise, hypothesis, correctAnswer) <- reader.read()) {
      totalCount += 1
      println(id)
      println("  premise: " + premise.mkString(" "))
      println("  hypothesis: " + hypothesis)
      val List(p, h) = mdi.batchInterpretMultisentence(List(premise, List(hypothesis)), verbose = false)
      if (p.isEmpty)
        println("  Failed to parse premise")
      if (h.isEmpty)
        println("  Failed to parse hypothesis")
      if (p.isDefined && h.isDefined) {
        parsedCount += 1
        val correct = correctAnswer.isDefined && correctAnswer.get
        val (proof, visual) = vmtp.proveVisualize(List(p.get), h.get)
        if (proof.isDefined == correct)
          correctCount += 1
        println("  result=%s, correct=%s %s".format(proof.isDefined, correct, if (proof.isDefined == correct) "" else "***********"))
        println("  total=%d, parsed=%d (%.2f), correct=%s (%.2f) (%.2f)".format(
          totalCount,
          parsedCount, parsedCount.toDouble / totalCount,
          correctCount, correctCount.toDouble / parsedCount, correctCount.toDouble / totalCount))
        println(sideBySideCentering("  ", visual))
        val List(premiseDisc, hypDisc) = cdp.batchParseMultisentence(List(premise, List(hypothesis)), model = Some("boxer")).map(_.get)
        println(sideBySideCentering(box(premiseDisc.repr), "  ", box(hypDisc.repr)))
      }
    }

  }
}
