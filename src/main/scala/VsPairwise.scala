import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.Pattern.Range
import utcompling.mlnsemantics.vecspace._
import utcompling.mlnsemantics.inference._
import java.text.DecimalFormat

    
object VsPairwise {
     
  
/*  def sim(word1:String, word2:String):Double = 
  {
     rw.weightForRules(word1, List(word1), Map(word2-> List(word2)), vectorspace).head._2.get
  }
 */ 
    def main(args: Array[String]) :Unit = {

      val lemFile = args(0);  
		val stsVsFile = args(1);
		val rw: RuleWeighter = (
			if(args(2) == "add")
				AwithCvecspaceWithSpellingSimilarityRuleWeighter(SimpleCompositeVectorMaker());
			else if (args(2) == "mul")
				AwithCvecspaceWithSpellingSimilarityRuleWeighter(MultiplicationCompositeVectorMaker());
			else {
				println ("wrong input %s".format(args(2)));
				return;
			}
		)
		val startFrom: Int = args(3).toInt;

      var vectorspace = BowVectorSpace(stsVsFile)
 	
        val allLemmas = readLines(lemFile) //.flatMap(_.split("\\s+"))
        val line  = ""
			var counter = 0;
        allLemmas.foreach(line => {
			try 
			{
					counter = counter + 1;
					if (counter < startFrom)
					{
						println("skip %s".format(counter));
					}
					else
					{
					  val splits = line.split("\t");
					  val txt = splits.apply(0);
					  val hyp = splits.apply(1);
					  val txtWords = txt.split(" ").map(s=>s.toLowerCase());
					  val hypWords = hyp.split(" ").map(s=>s.toLowerCase());
					  val words = txtWords ++ hypWords 
					  vectorspace = BowVectorSpace(stsVsFile, x => words.contains(x) )
					  val sim = rw.weightForRules(txtWords.mkString(" "), txtWords, Map(hypWords.mkString(" ")-> hypWords), vectorspace)
					  println("%1.4f".format(sim.head._2.get));
					}
			} catch 
			{
				case e: Exception =>{
					println("NaN");	
				}   				 
			}
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
