package utcompling.mlnsemantics.run

import utcompling.scalalogic.inference.impl.Prover9TheoremProver
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.scalalogic.util.FileUtils
import utcompling.scalalogic.util.CollectionUtils._
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.mlnsemantics.modal.ModalDiscourseInterpreter
import utcompling.mlnsemantics.wordnet.WordnetImpl
import utcompling.mlnsemantics.vecspace.BowVectorSpace
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.UnnecessarySubboxRemovingBoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.OccurrenceMarkingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.BoxerExpressionInterpreter
import org.apache.log4j.Logger
import org.apache.log4j.Level
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.PredicateCleaningBoxerExpressionInterpreterDecorator
import utcompling.mlnsemantics.inference._
import utcompling.mlnsemantics.datagen.Tokenize
import utcompling.mlnsemantics.datagen.CncLemmatizeCorpusMapper
import scala.io.Source

object Sts {

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)

    args.toSeq match {
      case Seq("lem", stsFile, lemFile) =>
        val sentences = Source.fromFile(stsFile).getLines.flatMap(_.split("\t")).toVector
        val lemmatized = new CncLemmatizeCorpusMapper().parseToLemmas(sentences)
        FileUtils.writeUsing(lemFile) { f =>
          lemmatized
            .map(_.map(_.map(_._2).mkString(" ")).getOrElse("______________"))
            .grouped(2).foreach { case Seq(a, b) => f.write("%s\t%s\n".format(a, b)) }
        }

      case Seq("vs", fullVsFile, lemFile, stsVsFile) =>
        
        
      case Seq("do") =>
        val vsf2 = (words: (String => Boolean)) => BowVectorSpace("resources/nytgiga.lem.1m.vc.f2000.m50.wInf", words)
        val runner = new RunFull(vsf2)
        runner.run("A man is riding a bicycle.", "A man is riding a bike.")

    }
  }

}
