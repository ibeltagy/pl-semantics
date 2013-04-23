package utcompling.scalalogic.discourse.candc.call
import scala.collection.mutable.ListBuffer

trait Boxer {

  //return BoxerOutput and list of parsing accuracies  
  def callBoxer(candcOut: String, args: Map[String,String] = Map(), verbose: Boolean = false): String//(String, ListBuffer[String])

}