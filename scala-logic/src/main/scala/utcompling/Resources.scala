package utcompling


class Resources  {
  
}

object Resources {
	
  var fullVectorSpace = "resources/full.vs";
  var candc = "candc/bin";
  var alchemy = "alchemy/bin";
  var polarityLexicon = "resources/polarity-lexicon/polarity_lexicon_expanded.txt";
  var wordnet = "resources/wordnet";
  
  def setVar(varName: String, varValue: String) = 
  {
    varName match {
      case "fullVectorSpace" => fullVectorSpace = varValue;
      case "candc" => candc = varValue;
      case "alchemy" => alchemy = varValue;
      case "polarityLexicon" => polarityLexicon = varValue;
      case "wordnet" => wordnet = varValue;
      case _ => throw new RuntimeException("unknown resource");
    }
  }

}
