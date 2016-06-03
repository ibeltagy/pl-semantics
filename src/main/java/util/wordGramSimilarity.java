package util;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class wordGramSimilarity {

	/**
	 * @param args
	 */
	static String concatinateRange(String [] words, int from, int len)
	{
		String concat = words[from];
		for (int i = from+1;i < from+len;i++)
		{
			concat = concat + "_" + words[i] ;
		}		
		return concat;
	}
	
	static String[] filterStopwords(String [] words, HashSet<String> sw)
	{
		ArrayList<String> filteredWords = new ArrayList<String>();
		for (String w: words)
		{
			if (! sw.contains(w))
				filteredWords.add(w);
		}		
		Object[] f = filteredWords.toArray();
		String[] stringArray = Arrays.copyOf(f, f.length, String[].class);
		return stringArray;
	}
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		String filePath = "/home/beltagy/workspace/deft/mln-semantics/resources/sts/STS.MSRpar.in.lem";
		String stopWordsFilePath = "stopwords.txt";
		try {
			BufferedReader swfr = new BufferedReader(new FileReader(stopWordsFilePath));
			String sw;
			HashSet<String> stopWords = new HashSet<String>();
			while ((sw = swfr.readLine()) != null)
			{
				stopWords.add(sw);
			}
			swfr.close();
			
			BufferedReader fr = new BufferedReader(new FileReader(filePath));

			String l;
			int section = 0; //0: any section
							//1: combination function
			String currentPair = "";
			int count = 0;
			while ((l = fr.readLine()) != null)
			{
				String [] pair = l.split("\t");
				String [] txt = filterStopwords(pair[0].split(" "), stopWords);
				String [] hyp = filterStopwords(pair[1].split(" "), stopWords);
				for (int ngramSize = 1; ngramSize <=4; ngramSize ++)
				{
					HashSet<String> txtNgrams = new HashSet<String>();
					HashSet<String> hypNgrams = new HashSet<String>();
					for (int i = 0; i < txt.length-ngramSize+1; i++)
						txtNgrams.add( concatinateRange(txt, i, ngramSize) );
					for (int i = 0; i < hyp.length-ngramSize+1; i++)
						hypNgrams.add( concatinateRange(hyp, i, ngramSize) );
					
					Set<String> union = new HashSet<String>(txtNgrams);
					union.addAll(hypNgrams);

					Set<String> intersection = new HashSet<String>(txtNgrams);
					intersection.retainAll(hypNgrams);
				     DecimalFormat df = new DecimalFormat("#.####");
				    double score = intersection.size() * 1.0 / union.size();
				    if (union.size() == 0)
				    	score = 0;
					System.out.print(df.format(score)  + ", ");

				}
				System.out.println(";");
				count ++;
				if (count == 457)
					count = 457;
				//System.out.println(countMatch + "," + txt.length +"," + hyp.length+";");
			}
			fr.close();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
