package utcompling.scalalogic.discourse.candc.call.impl

import utcompling.scalalogic.discourse.candc.call._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.FileUtils.pathjoin
import utcompling.scalalogic.util.SubprocessCallable
import utcompling.Resources

class CandcImpl	extends Candc {
    
    private val binary:String = FileUtils.findBinary(CandcImpl.binaryName, Some(Resources.candc) , Some("CANDCHOME"))
    private val modelsPath: String = pathjoin(binary.dropRight(5), "../models")
    private val defaultArgs: Map[String, String] = CandcImpl.extraArgs;

    //def this( defaultArgs: Map[String, String] = Map()) =
    //    this( , defaultArgs)

    override def batchParseMultisentence(inputs: Seq[Seq[String]], args: Map[String, String] = Map(), discourseIds: Option[Seq[String]] = None, model: Option[String] = None, verbose: Boolean = false): String = {
        val newDiscourseIds = discourseIds.getOrElse((0 until inputs.length).map(_.toString))
        val defaultArgs = Map[String, String](
            "--models" -> pathjoin(this.modelsPath, model.getOrElse("")))
        var allArgs = this.defaultArgs ++ defaultArgs ++ args
		if (CandcImpl.binaryName == "soap_client")
			allArgs = CandcImpl.extraArgs
        var caller = new SubprocessCallable(binary);
        return caller.call(Some(this.makeInput(inputs, newDiscourseIds)), (allArgs).flatMap { case (k, v) => List(k, v) }.toList, verbose)
    }

    private def makeInput(inputs: Seq[Seq[String]], discourseIds: Seq[String]): String = {
        require(inputs.length == discourseIds.length, "Must have the same number of inputs and discourseIds")
        val discourses = for ((d, id) <- (inputs zipSafe discourseIds)) yield "<META>'%s'".format(id) +: d
        return discourses.flatten.mkString("\n")
    }

}

object CandcImpl {
    var binaryName: String = "candc"  //can be changed in util.Config to soap_client 
    var extraArgs: Map[String, String] = Map(); //can be changed in util.Config to --url localhost:9000
	
/*
    def findBinary(binDir: Option[String] = None, envar: Option[String] = Some("CANDCHOME"), defaultArgs: Map[String, String] = Map(), verbose: Boolean = false) = {
        new CandcImpl(FileUtils.findBinary("candc", binDir, envar, verbose), defaultArgs = defaultArgs)
    }

    def findBinary(binDir: Option[String], envar: Option[String], modelsPath: String, defaultArgs: Map[String, String], verbose: Boolean) = {
        new CandcImpl(FileUtils.findBinary("candc", binDir, envar, verbose), modelsPath, defaultArgs)
    }
*/
}
