import scala.io._
import java.io._
import scala.util.Random

// scala classify.scala 1
val version = args(0)
val v1 = Source.fromFile("RTE" + version + ".train.result").getLines.toList
val v2 = Source.fromFile("RTE" + version + ".train.gs").getLines.toList
val v3 = Source.fromFile("RTE" + version + ".test.result").getLines.toList
val v4 = Source.fromFile("RTE" + version + ".test.gs").getLines.toList

var bestTrain = 0.0
var threshold = 0.0
var numPosCorrect = 0
var numNegCorrect = 0
var numPosIncorrect = 0
var numNegIncorrect = 0
var numCorrect = 0
var numInst = 0
var partialCover = 0.0

var numPos = 0
var cws = 0.0
var numCorrectUpRank = 0

for(prob <- 0.2 to 0.5 by 0.001)
{
	numPos = 0
	var cwsTrain = List[(Double, Boolean)]()
	for (i <- 0 to v1.length - 1)
	{
		if(v2(i).equals("1")) numPos += 1
		if(v1(i).toDouble >= 0) 
		{
			numInst += 1
			var isCorrect = false
		
			if(v1(i).toDouble >= prob) 
			{
				if(v2(i).equals("1"))
				{
					numPosCorrect += 1
					isCorrect = true
				}
				else numPosIncorrect += 1
			}
			else
			{
				if(v2(i).equals("0"))
				{
					numNegCorrect += 1
					isCorrect = true
				}
				else numNegIncorrect += 1
			}
			cwsTrain :+= (math.abs(v1(i).toDouble - prob), isCorrect)
		}
	}

	numCorrect = numPosCorrect + numNegCorrect

	if(bestTrain < numCorrect.toDouble / numInst)
	{
		bestTrain = numCorrect.toDouble / numInst
		threshold = prob
		println("\ntrain = " + prob)
		println("numCorrects = " + numPosCorrect + " + " + numNegCorrect + " = " + numCorrect)
		println("numPosIncorrects = " + numPosIncorrect)
		println("numNegIncorrects = " + numNegIncorrect)
		println("numClassifiedPairs = " + numInst)
		println("acc = " + bestTrain)
		partialCover = 100 * numInst.toDouble / v1.length
		cws = 0.0
		cwsTrain = cwsTrain.sortBy(x => x._1).reverse
		for(i <- 0 to cwsTrain.size - 1)
		{
			if(cwsTrain(i)._2) numCorrectUpRank += 1
			cws += numCorrectUpRank.toDouble / (i + 1)
		}
		cws /= cwsTrain.size
		println("cws = " + cws)
		numCorrectUpRank = 0
	}
	
	numPosCorrect = 0
	numNegCorrect = 0
	numPosIncorrect = 0
	numNegIncorrect = 0
	numInst = 0
}

println("numPairs = " + v1.length)
println("partialCover = " + partialCover + "%")
//println("numPos = " + numPos)
println("\n*************** RTE-" + version + " ***************\n")

numPos = 0
partialCover = 0.0
cws = 0.0
var cwsTest = List[(Double, Boolean)]()
var unsolvedPairs = List[Int]()

for (i <- 0 to v3.length - 1)
{
	if(v4(i).equals("1")) numPos += 1
	if(v3(i).toDouble >= 0)
	{
		numInst += 1
		var isCorrect = false
	
		if(v3(i).toDouble >= threshold) 
		{
			if(v4(i).equals("1"))
			{
				numPosCorrect += 1
				isCorrect = true
			}
			else 
			{
				numPosIncorrect += 1
				//print((i + 1) + ",")
			}
		}
		else
		{
			if(v4(i).equals("0"))
			{
				numNegCorrect += 1
				isCorrect = true
			}
			else 
			{
				numNegIncorrect += 1
				//print((i + 1) + ",")
			}
		}
		cwsTest :+= (math.abs(v3(i).toDouble - threshold), isCorrect)
	}
	else unsolvedPairs :+= v4(i).toInt
}

numCorrect = numPosCorrect + numNegCorrect
val bestTest = numCorrect.toDouble / numInst
println("\ntest = " + threshold)
println("numCorrects = " + numPosCorrect + " + " + numNegCorrect + " = " + numCorrect)
println("numPosIncorrects = " + numPosIncorrect)
println("numNegIncorrects = " + numNegIncorrect)
println("numClassifiedPairs = " + numInst)
println("numPairs = " + v3.length)
//println("numPos = " + numPos)

partialCover = 100 * numInst.toDouble / v3.length
println("partialCover = " + partialCover + "%")

println("\nacc = " + bestTest)

cwsTest = cwsTest.sortBy(x => x._1).reverse
for(i <- 0 to cwsTest.size - 1)
{
	if(cwsTest(i)._2) numCorrectUpRank += 1
	cws += numCorrectUpRank.toDouble / (i + 1)
}

val cws1 = cws / cwsTest.size
println("cws = " + cws1 + "\n")
/*
// Random predictions
val rand = new Random
var index = 0
for(i <- numInst to v3.length - 1)
{
//	if(rand.nextDouble >= 0.5)
	if(unsolvedPairs(index) == 0)
	{
		numCorrect += 1
		numCorrectUpRank += 1
	}
	index += 1
	cws += numCorrectUpRank.toDouble / (i + 1)
}
val tmp = numCorrect.toDouble / v3.length
println("tmp = " + tmp)

cws /= v3.length
println("cws1 = " + cws)

println("numParseErrs = " + numParseErrs)
*/



