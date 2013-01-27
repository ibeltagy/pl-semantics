package utcompling.scalalogic.discourse.candc.call.impl

import utcompling.scalalogic.discourse.candc.call.Boxer
import opennlp.scalabha.util.FileUtils
import utcompling.scalalogic.util.SubprocessCallable

class BoxerImpl(override val binary: String, defaultArgs: Map[String, String] = Map()) extends SubprocessCallable(binary) with Boxer {

    override def callBoxer(candcOut: String, args: Map[String, String] = Map(), verbose: Boolean = false): String = {
        val tempFilename = FileUtils.mktemp(prefix = "boxer-", suffix = ".in")

        FileUtils.writeUsing(tempFilename) { f =>
            f.write(candcOut)
        }

        val defaultArgs = Map[String, String](
            "--input" -> tempFilename)
        val stdout = this.call(None, (this.defaultArgs ++ defaultArgs ++ args).flatMap { case (k, v) => List(k, v) }.toList, verbose)
        FileUtils.remove(tempFilename)
        return stdout
    }

}

object BoxerImpl {

    def findBinary(binDir: Option[String] = None, envar: Option[String] = Some("CANDCHOME"), defaultArgs: Map[String, String] = Map(), verbose: Boolean = false) =
        new BoxerImpl(FileUtils.findBinary("boxer", binDir, envar, verbose), defaultArgs)

}
