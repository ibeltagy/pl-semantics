import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.Vector;


class Predicate
{
	String name; 
	String prob;
	Vector < Integer> varIndx;
	Vector < String> varName;
	Vector < String> varVal;	
	
}


public class Main {

	/**
	 * @param args
	 */
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		String filePath = "/u/beltagy/workspace/deft/mln-semantics/data/withRelationOneEntWeightsselectionPrior";
		
		try {
			BufferedReader fr = new BufferedReader(new FileReader(filePath));
			Vector<String> errors = new Vector<String>();
			String l;
			int section = 0; //0: any section
							//1: combination function
			String currentPair = "";
			int n = 0;
			Vector<Predicate> predicates = new Vector<Predicate>();
			Map<String, Integer> variables = new HashMap <String, Integer>();
			
			StringBuilder resultFile = null;
			StringBuilder evdFile = null;
			double maxProb = -1;
			String bestWorld = null;
			
			while ((l = fr.readLine()) != null)
			{
		//	try
			//{
				if(l.startsWith("  Pair"))
				{
					predicates.clear();
					n = 0;
					variables.clear();
					currentPair = l.substring(7);
					resultFile = new StringBuilder();
					evdFile = new StringBuilder();
					maxProb = -1;
					bestWorld = "";
					if(currentPair.contains("4"))
					{
						String x = "";
						x = currentPair;
						
					}
				}
				else if (l.startsWith("//begin combination function"))
				{
					section = 1;
					
				}
				else if (l.startsWith("//end combination function"))
				{
					section = 0;
				}
				else if (section == 1)
				{
					Predicate pred = new Predicate();
					pred.varIndx = new Vector<Integer>();
					pred.varName = new Vector<String>();
					pred.varVal = new Vector<String>();
					pred.prob = "N/A";
					pred.name = "N/A";
					
					Scanner s = new Scanner(l);
					s.nextDouble();
					
					String p = s.next();
					try{
					if (variables.size() == 0)
					{
						String skip = s.next();
						skip = s.next();						
						String [] allVariables = skip.substring(22).replace(")", "").split(",");
						for(int i = 0; i<allVariables.length ; i++)
						{
							variables.put(allVariables[i], i);
						}
						
					}
					}catch(Exception e)
					{
						variables.clear();
						//System.err.println(currentPair + "error");
						errors.add(currentPair);
					}
					
					try{					
					String [] splits = p.substring(1).split("\\(");
					pred.name = splits[0];					
					String vars = splits[1].replace(")", "");
					splits = vars.split(",");
					for (String v:splits)
					{
						Integer idx = variables.get(v);
						if (idx == null)
						{
							variables.put(v, variables.size());
							idx = variables.get(v);
							//System.err.println(currentPair + "missing var");
							errors.add(currentPair);
						}
						pred.varIndx.add(idx);
						pred.varName.add(v);
						pred.varVal.add("N/A");
						
					}
					}catch(Exception e)
					{
						//System.err.println(currentPair + "error");
						errors.add(currentPair);
					}
					predicates.add(pred);
					//predicates.put(pred.name, pred);
					n++;
				}
				else if (l.contains("AlchemyTheoremProver results file"))
				{
					section = 2;
				}
				else if (l.startsWith("Some("))
				{
					String[] outputLine = l.split(" ");
					double actProb = Double.parseDouble(outputLine[3].replace(",", ""));
					double gtProb = Double.parseDouble(outputLine[5].replace("]", ""));
					//Process bestWorld
					String [] bestVarAssignment = bestWorld.split(",");

					//process evd file
					String[] evdLines = evdFile.toString().split("\n");
					for (String evdFileLine:evdLines )
					{
						String[] splits = evdFileLine.split("\\(");
						String predicateName = splits[0];
						//Collection<Predicate> coll = (Collection<Predicate>)predicates.get(splits[0]);
						//if (coll == null)
							//continue;
 
						splits = splits[1].split("\\)");
						String [] predVars = splits[0].split(",");
						for (Predicate p : predicates)
						{
							if (!p.name.equals(predicateName))
								continue;
							boolean matchs = true;
							for (int i = 0;i <predVars.length;i++)
							{
								if (p.varIndx.get(i) >= bestVarAssignment.length)
								{
									if (p.varName.get(i).equals(predVars[i]))
										p.varVal.set(i, predVars[i]);
									else 
									{
										matchs = false;
										break;
									}
								}
								else if (bestVarAssignment[p.varIndx.get(i)].equals(predVars[i]))
								{
									p.varVal.set(i, predVars[i]);
								}
								else
								{
									matchs = false;
									break;
								}
							}
							if (matchs)
							{
								p.prob = "1.0";//splits[1].trim();
							}						
						}
					}

					
					//process result file
					String[] lines = resultFile.toString().split("\n");
					for (String resFileLine:lines)
					{
						String[] splits = resFileLine.split("\\(");
						String predicateName = splits[0];
						//Collection<Predicate> coll = (Collection<Predicate>)predicates.get(splits[0]);
						//if (coll == null)
//							continue;
						
						splits = splits[1].split("\\)");
						String [] predVars = splits[0].split(",");
						for(Predicate p: predicates)						
						{
							if (!p.name.equals(predicateName))
								continue;
							boolean matchs = true;
							for (int i = 0;i <predVars.length;i++)
							{
								if (p.varIndx.get(i) >= bestVarAssignment.length)
								{
									if (p.varName.get(i).equals(predVars[i]))
										p.varVal.set(i, predVars[i]);
									else 
									{
										matchs = false;
										break;
									}
								}
								else if (bestVarAssignment[p.varIndx.get(i)].equals(predVars[i]))
								{
									p.varVal.set(i, predVars[i]);
								}
								else
								{
									matchs = false;
									break;
								}
							}
							if (matchs)
							{
								p.prob = splits[1].trim();
							}
						}
					}
					section = 0;
					
					//print output
					System.out.print(currentPair + "\t" + gtProb/5.0 + "\t" + actProb/5.0 + "\t" );
					System.out.print(predicates.size() + "\t");
					if (n != predicates.size() )
					{
						System.out.print ("----------------n=" + n + "  pred=" + predicates.size() + "\n");
					}	
				
					
					String str = "";
					for (Predicate p: predicates)
					{
						String prob = p.prob; 
						if (prob.equals("N/A"))
						{
							prob = "0";
							errors.add(currentPair);
						}
						str  = str  + prob + ", ";
					}
					
					for (int i = predicates.size() ; i <= 31;i++)
					{
						str  = str  +  " 0, ";
					}
					
					str = str.substring(0, str.length()-1);
					//System.out.print(str);
					System.out.print(" ; \n");				
					
				}
				else if (section == 2)
				{
					if (l.startsWith("entailment"))
					{
						String[] splits = l.split(" ");
						double prob = Double.parseDouble(splits[1]);
						if (prob > maxProb)
						{
							maxProb = prob;
							bestWorld = splits[0].substring(22).replace(")", "");							
						}
					}
					else
					{
						resultFile.append(l+"\n");
					}
				}
				else if (l.contains("AlchemyTheoremProver evidence file"))
				{
					section = 3;
				}
				else if (l.startsWith("Calling: "))
				{
					section = 0;
				}
				else if (section == 3)
				{
					evdFile.append(l+"\n");
				}

		/*	}
			catch(Exception e)
			{
				predicates.clear();
				variables.clear();
				currentPair = "failed";
				resultFile = new StringBuilder();
				evdFile = new StringBuilder();
				maxProb = -1;
				bestWorld = "";	
			}*/
			}
			//System.out.println(errors.toString());
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
