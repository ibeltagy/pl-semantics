import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.text.DecimalFormat;
import java.util.*;

import edu.cmu.lti.lexical_db.ILexicalDatabase;
import edu.cmu.lti.lexical_db.NictWordNet;
import edu.cmu.lti.ws4j.RelatednessCalculator;
import edu.cmu.lti.ws4j.impl.Resnik;
import edu.cmu.lti.ws4j.util.WS4JConfiguration;
import utcompling.mlnsemantics.vecspace.*;
import utcompling.mlnsemantics.inference.*;

//import edu.cmu.lti.ws4j.util.WordSimilarityCalculator;

public class PairwiseWordSimilarity {

		/**
		 * @param args
		 */
		static double maxMaxSim = 0;
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
		
		public static ILexicalDatabase db = new NictWordNet();
	    public static RelatednessCalculator rcs = new Resnik(db);
	    public static int docsCount = 1;
    	
	    public static BowVectorSpace vectorspace = null;// new BowVectorSpace(stsVsFile, x => words.contains(x) )
	    public static RuleWeighter rw = new AwithCvecspaceWithSpellingSimilarityRuleWeighter(new MultiplicationCompositeVectorMaker());
   		//val sim = rw.weightForRules(txtWords.mkString("_"), txtWords, Map(hypWords.mkString("_")-> hypWords), vectorspace)
	    
	    public static double sentenceSim(String [] s1, String []s2, boolean diffOnly)
	    {
	    	double sumIdf = 0;
	    	double num = 0;
	    	for (String word1:s1)
	    	{
	    		word1 = word1.toLowerCase().replace("-", "");
	    		Integer df = dfMap.get(word1.toLowerCase().replace("-", ""));
	    		if (df == null)
	    		{
	    			df = 1;
	    			//System.out.println("missing word: " + word1);
	    		}

	    		double idf = Math.log(docsCount*1.0 / df.intValue());
	    		
	    		double maxSim = 0;
	    		boolean sameWordFound = false;
	    		for (String word2:s2)
	    		{
	    			word2 = word2.toLowerCase().replace("-", "");
	    			//double s = rcs.calcRelatednessOfWords(word1, word2);

					//TODO: I will fix the line below later
	    			//double s = VsPairwise.sim(word1, word2); 
					double s = 0.0;




//rw.weightForRules(word1, (Iterable<String>)Arrays.asList(s1), (Map<String, Iterable<String>>)new HashMap<String, ArrayList<String>>(), (Map<String, BowVector>)new HashMap<String,BowVector>());
	    			if(word1.equals(word2))
	    			{
	    				s = 12;
	    				sameWordFound = true;
	    			}
	    			if (s>maxSim)
	    				maxSim = s;
	    			if (s>maxMaxSim)
	    				maxMaxSim = s;
	    		}
	    		if (!(sameWordFound && diffOnly))
	    		{
	    			sumIdf += idf;
	    			num += maxSim*idf;
	    		}
	    	}	    	
	    	if (sumIdf != 0)
	    		return num/sumIdf;
	    	else return 12;
	    }
	    public static HashMap<String, Integer> dfMap ;
	    
		public static void main(String[] args) {
			// TODO Auto-generated method stub
			String filePath = "/home/beltagy/workspace/deft/mln-semantics/resources/sts/STS.MSRvid.in.lem";
	    	String stsVsFile = "/home/beltagy/workspace/deft/starsem/STS.MSRvid.in.vs";
			String stopWordsFilePath = "/home/beltagy/workspace/deft/mln-semantics/helpingTools/bagWords/stopwords.txt";
			String dfFilePath = "/home/beltagy/workspace/deft/mln-semantics/helpingTools/bagWords/df.txt";
			
			

            WS4JConfiguration.getInstance().setMFS(true);
            
	        double s = rcs.calcRelatednessOfWords("car", "train");
	        System.out.println( rcs.getClass().getName()+"\t"+s );
			
			
			try {
				BufferedReader swfr = new BufferedReader(new FileReader(stopWordsFilePath));
				String sw;
				HashSet<String> stopWords = new HashSet<String>();
				while ((sw = swfr.readLine()) != null)
				{
					stopWords.add(sw);
				}
				swfr.close();
				
				BufferedReader dffr = new BufferedReader(new FileReader(dfFilePath));
				String df;
				dfMap = new HashMap<String, Integer> ();
				int totalCount = 0;
				while ((df = dffr.readLine()) != null)
				{
					try{
						String[] splits = df.trim().split("\t");
						int f = Integer.parseInt(splits[1]);
						totalCount += f;
						dfMap.put(splits[0], 5);
					}catch (Exception e)
					{
						e.printStackTrace();
					}
					
				}
				dffr.close();
				
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
					
					//vectorspace = new BowVectorSpace();
					//vectorspace. x => words.contains(x)

					double score1 = sentenceSim(txt, hyp, false);
					double score2 = sentenceSim(hyp, txt, false);
					
					DecimalFormat dfrm = new DecimalFormat("#.####");
				    double score = (score1 + score2) / 2.0;
					/*System.out.print(dfrm.format(score) + ", ");
					
					
					
					double score3 = sentenceSim(txt, hyp, true);
					double score4 = sentenceSim(hyp, txt, true);
					
					dfrm = new DecimalFormat("#.####");
				    score = (score3 + score4) / 2.0;
				    */
					System.out.print(dfrm.format(score));
					
					System.out.println(";");
					count ++;
					//System.out.println(countMatch + "," + txt.length +"," + hyp.length+";");
				}
				fr.close();
				System.out.print(maxMaxSim);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
	}
