package utcompling.scalalogic.util.subprocess

import scala.sys.process._
import opennlp.scalabha.util.FileUtils
import org.junit.Test

class SubprocessTests {
  def test() {

    {
      println("""echo "this": pb ! ProcessLogger""")
      // "!" blocks, returns error code, and logs with ProcessLogger
      val pb = Process("""echo "this"""")
      val out = new StringBuilder
      val err = new StringBuilder
      val exitcode = pb ! ProcessLogger(out.append(_), err.append(_))
      println("stdout: " + out.result)
      println("stderr: " + err.result)
      println()
    }
    {
      println("""echo "this": pb !!""")
      // "!!" blocks, returns stdout, and throws an exception
      val pb = Process("""echo "this"""")
      val out = pb !!;
      println("stdout: " + out)
      println()
    }

    {
      try {
        println("""cat blah: pb !!""")
        // "!!" blocks, returns stdout, prints stderr, and throws an exception when exitcode != 0
        val pb = Process("""cat blah""")
        val out = pb !!;
        println("stdout: " + out)
      } catch {
        case e => println("Exception:\n" + e)
      }
      println()
    }

    {
      println("""cat < {"this"}: pb.run(io): BAD""")
      //THIS FAILS BECAUSE IT DOESN'T BLOCK
      val pb = Process("cat")
      val out = new StringBuilder
      val err = new StringBuilder
      val io = new ProcessIO(
        i => { i.write("this" getBytes "UTF-8"); i.close },
        o => { scala.io.Source.fromInputStream(o).getLines.foreach(out.append) },
        e => { scala.io.Source.fromInputStream(e).getLines.foreach(err.append) })
      pb.run(io)
      println("stdout: " + out.result)
      println("stderr: " + err.result)
      println()
    }

    {
      println("""cat < {"this"}: pb.run(io)""")
      val pb = Process("cat")
      val out = new StringBuilder
      val err = new StringBuilder
      val exitcode = pb #< new java.io.ByteArrayInputStream("this".getBytes("UTF-8")) ! ProcessLogger(out.append(_), err.append(_))
      println("stdout: " + out.result)
      println("stderr: " + err.result)
      println()
    }

    {
      println("""cat < {"this"}: pb.run(io)""")
      val pb = Process("cat")
      val out = new StringBuilder
      val err = new StringBuilder
      val exitcode = pb #< new java.io.File(FileUtils.pathjoin(System.getenv("HOME"), "temp")) ! ProcessLogger(out.append(_), err.append(_))
      println("stdout: " + out.result)
      println("stderr: " + err.result)
      println()
    }

    {
      println("""echo "this" | cat: pb.run(io)""")
      val echoP = Process("""echo "this"""")
      val catP = Process("""cat""")
      val out = new StringBuilder
      val err = new StringBuilder
      val exitcode = echoP #| catP ! ProcessLogger(out.append(_), err.append(_))
      println("stdout: " + out.result)
      println("stderr: " + err.result)
      println()
    }

    {
      println("""cat blah | cat: pb.run(io)""")
      val echoP = Process("""cat blah""")
      val catP = Process("""cat""")
      val out = new StringBuilder
      val err = new StringBuilder
      val exitcode = echoP #| catP ! ProcessLogger(out.append(_), err.append(_))
      println("stdout: " + out.result)
      println("stderr: " + err.result)
      println()
    }

    {
      println("""echo "this" | cat blah: pb.run(io)""")
      val echoP = Process("""echo "this"""")
      val catP = Process("""cat blah""")
      val out = new StringBuilder
      val err = new StringBuilder
      val exitcode = echoP #| catP ! ProcessLogger(out.append(_), err.append(_))
      println("stdout: " + out.result)
      println("stderr: " + err.result)
      println()
    }

    println("done")
  }
}
