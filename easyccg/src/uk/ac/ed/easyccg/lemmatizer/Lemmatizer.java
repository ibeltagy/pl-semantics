package uk.ac.ed.easyccg.lemmatizer;

import edu.stanford.nlp.process.Morphology;

public  class Lemmatizer {
	private static final Morphology morphology = new Morphology();
	public static String lemmatizeWord(String s ) 
	{
		return morphology.stem(s);
	}
	public static String lemmatizeWords(String s ) 
	{
		StringBuilder result = new StringBuilder();
		String[] splits = s.split(" ");
		for (String split:splits)
			if (result.length() == 0)
				result.append(lemmatizeWord(split));
			else
				result.append(" " + lemmatizeWord(split));
		return result.toString();
	}
}