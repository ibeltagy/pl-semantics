package utcompling.scalalogic.discourse.candc.call.impl

import utcompling.scalalogic.discourse.candc.call.Boxer
import opennlp.scalabha.util.FileUtils
import utcompling.scalalogic.util.SubprocessCallable
import scala.sys.process.Process
import scala.sys.process.ProcessLogger
import scala.collection.mutable.ListBuffer
import utcompling.Resources
import opennlp.scalabha.util.FileUtils._
import opennlp.scalabha.util.FileUtils

//class BoxerImpl(override val binary: String, defaultArgs: Map[String, String] = Map()) extends SubprocessCallable(binary) with Boxer {
class BoxerImpl extends  Boxer {

	private val binary:String = FileUtils.findBinary("boxer", Some(Resources.candc) , Some("CANDCHOME"))
	private val defaultArgs: Map[String, String] = Map();
	
    override def callBoxer(candcOut: String, args: Map[String, String] = Map(), verbose: Boolean = false): String/*(String, ListBuffer[String])*/ = {
        val tmpInFilename = FileUtils.mktemp(prefix = "boxer-", suffix = ".in")
        val tmpOutFilename = FileUtils.mktemp(prefix = "boxer-", suffix = ".out")

        FileUtils.writeUsing(tmpInFilename) { f =>
            f.write(candcOut)
        }
        
        /*val out = new ListBuffer[String]
		val err = new StringBuilder
	 
	    var command = "grep '%  score = ' "+ tempFilename
	    Process("/bin/sh", Seq("-c", command)) ! (ProcessLogger(l => out.append(l), System.err.println(_)))
	    
	    //println (out.mkString("\n"))
	     */

        val defaultArgs = Map[String, String](
            "--input" -> tmpInFilename,
            "--output" -> tmpOutFilename)
        var caller = new SubprocessCallable(binary);
        caller.call(None, (this.defaultArgs ++ defaultArgs ++ args).flatMap { case (k, v) => List(k, v) }.toList, true)
        val stdout = readLines(tmpOutFilename).toList.mkString("\n");
        FileUtils.remove(tmpInFilename)
        FileUtils.remove(tmpOutFilename)
        //return (stdout, out)
        return stdout
    }

}

object BoxerImpl {

    //def findBinary(binDir: Option[String] = None, envar: Option[String] = Some("CANDCHOME"), defaultArgs: Map[String, String] = Map(), verbose: Boolean = false) =
    //    new BoxerImpl(FileUtils.findBinary("boxer", binDir, envar, verbose), defaultArgs)

}
