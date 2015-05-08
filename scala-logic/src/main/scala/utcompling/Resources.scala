package utcompling


class Resources  {
  
}

object Resources {
	
  var fullVectorSpace = "resources/full.vs";
  var candc = "candc/bin";
  var alchemy = "alchemy/bin";
  var polarityLexicon = "resources/polarity-lexicon/polarity_lexicon_expanded.txt";
  var wordnet = "resources/wordnet";
  var sureRules = "resources/sure" //manually annotated rules from the SICK-RTE dataset
  var trueRules = "resources/true" //all Entailing rules in the traning set of the SICK-RTE dataset. 

  
  def setVar(varName: String, varValue: String) = 
  {
    varName match {
      case "fullVectorSpace" => fullVectorSpace = varValue;
      case "candc" => candc = varValue;
      case "alchemy" => alchemy = varValue;
      case "polarityLexicon" => polarityLexicon = varValue;
      case "wordnet" => wordnet = varValue;
      case "sureRules" => sureRules = varValue;
      case "trueRules" => trueRules = varValue;
      case _ => throw new RuntimeException("unknown resource");
    }
  }

}
