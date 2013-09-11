import scala.io._
import java.io._

// scala multiParses.scala test 1
val trainOrTest = args(0)
val version = args(1)

println("RTE-" + version + "-" + trainOrTest)

val writer = new PrintWriter(new File("RTE" + version + "." + trainOrTest + ".result"))

val gslines = Source.fromFile("RTE" + version + "." + trainOrTest + ".gs").getLines.toSeq

val parseIdxes =  Array(1)
		// Array(1, 2, 4, 5)
		// Array(1, 2, 3, 6, 7, 8, 11, 12, 13)
		// Array(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
		// Array(1, 2, 3, 4, 5, 6, 10, 11, 13, 14)
		//1 to 9

val (beginIdx, endIdx) = (1, gslines.length)

for(fileIdx <- beginIdx to endIdx)
{
	var scores = List[Double]()
	var probs = List[Double]()
	var score = -5.0
	var result = -5.0
	for(parseIdx <- parseIdxes)
	{
		try
		{
			val lines = Source.fromFile("multiOut-" + trainOrTest + 
						"/RTE" + version + "." + trainOrTest + "." + fileIdx + ".multiOut." + parseIdx)
					.getLines
					.toSeq

			val Array(textScore, hypScore, probStr, maxScore) = lines(0).split(" ")
			val prob = probStr.toDouble

			val oneScore = prob //* (textScore.toDouble + hypScore.toDouble)

			if(oneScore >= 0)
			{
				scores :+= oneScore
				probs :+= prob
			}
		}
		catch 
		{
    			case e: Exception => 
			{
				//println(e)
				//print(fileIdx + ",")
				//score = -3
				//result = -3
			}
    		} 	
	}

	if(scores.size > 0)
	{
		result = probs.max
	}

	writer.write(result + "\n")   			 
}

writer.close()

