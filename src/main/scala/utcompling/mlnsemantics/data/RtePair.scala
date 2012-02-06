package utcompling.mlnsemantics.data

case class RtePair(
        id: String,
        premise: List[String], 
        hypothesis: String,
        entailed: Option[Boolean]) {

}