import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.Pattern.Range
import utcompling.mlnsemantics.vecspace.BowVector
import utcompling.mlnsemantics.vecspace.BowVectorSpace
import utcompling.mlnsemantics.inference._
import java.text.DecimalFormat

    
object VsPairwise {
     
  val stsVsFile = "/home/beltagy/workspace/deft/mln-semantics/resources/sts/STS.MSRvid.in.vs";
  val rw: RuleWeighter = AwithCvecspaceWithSpellingSimilarityRuleWeighter(SimpleCompositeVectorMaker());
  var vectorspace = BowVectorSpace(stsVsFile)
  
  def sim(word1:String, word2:String):Double = 
  {
     rw.weightForRules(word1, List(word1), Map(word2-> List(word2)), vectorspace).head._2.get
  }
  
    def main(args: Array[String]) :Unit = {
      
    	val lemFile = "/home/beltagy/workspace/deft/mln-semantics/resources/sts/STS.MSRvid.in.lem";
    	
    	println("....")
        val allLemmas = readLines(lemFile) //.flatMap(_.split("\\s+"))
        val line  = ""

        allLemmas.foreach(line => {
        	val splits = line.split("\t");
        	val txt = splits.apply(0);
        	val hyp = splits.apply(1);
        	val txtWords = txt.split(" ").map(s=>s.toLowerCase());
        	val hypWords = hyp.split(" ").map(s=>s.toLowerCase());
        	val words = txtWords ++ hypWords 
        	vectorspace = BowVectorSpace(stsVsFile, x => words.contains(x) )
        	val sim = rw.weightForRules(txtWords.mkString("_"), txtWords, Map(hypWords.mkString("_")-> hypWords), vectorspace)
        	println("%1.4f".format(sim.head._2.get));
        	//val txtVec = txtWords.foreach(w=> { })
        	//val dfrm = DecimalFormat("#.####");
        	//println(dfrm.format(score));

        })

        println("hi");
        System.exit(0)
    }
    ////////////////////
    
    
    
    /////////////////////
}