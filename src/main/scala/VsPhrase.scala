import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.Pattern.Range
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.mlnsemantics.vecspace.BowVectorSpace
import utcompling.mlnsemantics.inference._
import java.text.DecimalFormat

    
object VsPhrase {
     
  
    def main(args: Array[String]) :Unit = {
      
    	val lemFile = "/home/beltagy/workspace/deft/starsem/RTE1/train/RTE1.train.lem";
    	val stsVsFile = "/home/beltagy/workspace/deft/starsem/STS.MSRvid.in.vs";
    	println("....")
        val allLemmas = readLines(lemFile) //.flatMap(_.split("\\s+"))
        val line  = ""
        val rw: RuleWeighter = AwithCvecspaceWithSpellingSimilarityRuleWeighter(MultiplicationCompositeVectorMaker());
        allLemmas.foreach(line => {
        	val splits = line.split("\t");
        	val txt = splits.apply(0);
        	val hyp = splits.apply(1);
        	val txtWords = txt.split(" ").map(s=>s.toLowerCase());
        	val hypWords = hyp.split(" ").map(s=>s.toLowerCase());
        	val words = txtWords ++ hypWords 
        	val vectorspace = BowVectorSpace(stsVsFile, x => words.contains(x) )
        	val sim = rw.weightForRules(txtWords.mkString("_"), txtWords, Map(hypWords.mkString("_")-> hypWords), vectorspace)
        	println("%1.4f".format(sim.head._2.get));
        	//val txtVec = txtWords.foreach(w=> { })
        	//val dfrm = DecimalFormat("#.####");
        	//println(dfrm.format(score));

        })

        println("hi");
        System.exit(0)
    }
}