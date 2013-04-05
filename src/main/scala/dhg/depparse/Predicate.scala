package dhg.depparse

case class Predicate(idx: Integer, name: String, varName: String, tag: String, typ: String, token: String, varName2: Option[String]=None) {
  
  override def toString: String = {
   varName2 match {
     case Some(s) => "%s(%s, %s)".format(name, varName, s)
     case _ => "%s(%s)".format(name, varName)
   } 
  }

}

object Predicate {
}

