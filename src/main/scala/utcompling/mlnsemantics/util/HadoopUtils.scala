package utcompling.mlnsemantics.util

import scala.collection.JavaConversions._
import org.apache.hadoop.fs.Path
import utcompling.mlnsemantics.util.HdfsFileSystemUtils._

object HadoopUtils {

  implicit def string2path(s: String) = new Path(s)

  def getMerge(path: String): String = {
    val filepath = new Path(path)
    if (isDirectory(filepath)) {
      val sb = new StringBuilder
      for (f <- listFiles(filepath).filterNot(_.getName.startsWith("."))) {
        //println("Reading from: " + f)
        val s = readFile(f).trim
        if (s.nonEmpty)
          sb.append(s + "\n")
      }
      return sb.dropRight(1).toString
    }
    else {
      return readFile(filepath)
    }
  }

  def using[T <: { def close() }, R](resource: T)(block: T => R): Option[R] = {
    try {
      Some(block(resource))
    }
    finally {
      if (resource != null)
        resource.close()
      None
    }
  }

}