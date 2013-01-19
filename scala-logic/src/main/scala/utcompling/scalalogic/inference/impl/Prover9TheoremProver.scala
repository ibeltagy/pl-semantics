package utcompling.scalalogic.inference.impl

import utcompling.scalalogic.inference.TheoremProver
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression._
import utcompling.scalalogic.util.SubprocessCallable
import opennlp.scalabha.util.FileUtils
import scala.collection.mutable.ListBuffer

class Prover9TheoremProver(override val binary: String, timeout: Int, errorOnExceed: Boolean)
    extends SubprocessCallable(binary)
    with TheoremProver[FolExpression, String] {

    override def prove(assumptions: List[FolExpression] = List(), goal: Option[FolExpression] = None, verbose: Boolean = false): Option[String] = {
        if (false) {
            assumptions.map(println)
            println
            println(goal)
            println
        }
        return this.callProver9(makeInput(goal, assumptions), verbose = verbose).map(this.getProof(_))
    }

    private def callProver9(input: List[String], args: List[String] = List(), verbose: Boolean = false): Option[String] = {
        val timeout = if (this.timeout > 0) List("assign(max_seconds, %d).".format(this.timeout)) else List()

        val inputStr = Some((input ++ timeout).mkString("\n"))
        val (exitcode, stdout, stderr) = this.callAllReturns(inputStr, args, verbose)

        val errormsg =
            if (stdout.contains("%%ERROR:"))
                Some(stdout.drop(stdout.indexOf("%%ERROR:")).trim())
            else
                None

        return exitcode match {
            case 0 => Some(stdout)
            case 2 => None
            case _ if List(3, 4, 5, 6).contains(exitcode) =>
                if (errorOnExceed)
                    throw new Prover9LimitExceededException(exitcode, errormsg)
                else
                    None
            case _ =>
                throw new Prover9FatalException(exitcode, errormsg)
        }
    }

    private def makeInput(goal: Option[FolExpression], assumptions: List[FolExpression]): List[String] = {
        val s = new ListBuffer[String]

        s += "clear(auto_denials)." //only one proof required
        s += ""

        if (assumptions.nonEmpty) {
            s += "formulas(assumptions)."
            for (a <- assumptions)
                s += "    %s.".format(convert(a))
            s += "end_of_list."
            s += ""
        }

        if (goal.isDefined) {
            s += "formulas(goals)."
            s += "    %s.".format(convert(goal.get))
            s += "end_of_list."
            s += ""
        }

        return s.result
    }

    private def convert(input: FolExpression): String =
        input match {
            case FolExistsExpression(variable, term) => "exists " + variable.name + " " + convert(term)
            case FolAllExpression(variable, term) => "all " + variable.name + " " + convert(term)
            case FolNegatedExpression(term) => "-(" + convert(term) + ")"
            case FolAndExpression(first, second) => "(" + convert(first) + " & " + convert(second) + ")"
            case FolOrExpression(first, second) => "(" + convert(first) + " | " + convert(second) + ")"
            case FolIfExpression(first, second) => "(" + convert(first) + " -> " + convert(second) + ")"
            case FolIffExpression(first, second) => "(" + convert(first) + " <-> " + convert(second) + ")"
            case FolEqualityExpression(first, second) => "(" + convert(first) + " = " + convert(second) + ")"
            case FolAtom(pred, args @ _*) => pred.name.replace("'", "") + "(" + args.map(_.name).mkString(",") + ")"
            case FolVariableExpression(Variable(v)) => v
        }

    private def getProof(stdout: String): String = {
        val proof = new ListBuffer[String]
        val proofRe = """(?s)=+ PROOF =+\n(.*)\n=+ end of proof =+""".r
        for (line <- proofRe.findFirstMatchIn(stdout).get.group(1).split("\n")) {
            line.trim() match {
                case "" => {}
                case _ if (line.startsWith("%")) => {}
                case _ => proof += line
            }
        }
        return proof.mkString("\n")
    }

}

object Prover9TheoremProver {

    def findBinary(binDir: Option[String] = None, envar: Option[String] = Some("PROVER9HOME"), timeout: Int = 60, errorOnExceed: Boolean = false, verbose: Boolean = false) =
        new Prover9TheoremProver(FileUtils.findBinary("prover9", binDir, envar, verbose), timeout, errorOnExceed)

}

class Prover9Exception(val exitcode: Int, val errorMsg: Option[String])
    extends RuntimeException(
        (exitcode match {
            case 1 => "(FATAL)" // A fatal error occurred (user's syntax error).
            case 2 => "(SOS_EMPTY)" // Prover9 ran out of things to do (sos list exhausted).
            case 3 => "(MAX_MEGS)" // The max_megs (memory limit) parameter was exceeded.
            case 4 => "(MAX_SECONDS)" // The max_seconds parameter was exceeded.
            case 5 => "(MAX_GIVEN)" // The max_given parameter was exceeded.
            case 6 => "(MAX_KEPT)" // The max_kept parameter was exceeded.
            case 7 => "(ACTION)" // A Prover9 action terminated the search.
            case 101 => "(SIGSEGV)" // Prover9 crashed, most probably due to a bug.   
        }) + ": " + errorMsg.map(_ + "\n").getOrElse(""))

class Prover9FatalException(override val exitcode: Int, override val errorMsg: Option[String]) extends Prover9Exception(exitcode, errorMsg)

class Prover9LimitExceededException(override val exitcode: Int, override val errorMsg: Option[String]) extends Prover9Exception(exitcode, errorMsg)
