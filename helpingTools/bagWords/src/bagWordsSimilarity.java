import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;

public class bagWordsSimilarity {

	/**
	 * @param args
	 */
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		String filePath = "/home/beltagy/workspace/deft/mln-semantics/resources/sts/STS.MSRpar.in.lem";
		
		try {
			BufferedReader fr = new BufferedReader(new FileReader(filePath));

			String l;
			int section = 0; //0: any section
							//1: combination function
			String currentPair = "";
		
			while ((l = fr.readLine()) != null)
			{
				String [] pair = l.split("\t");
				String [] txt = pair[0].split(" ");
				String [] hyp = pair[1].split(" ");
				int countMatch = 0;
				for (String t:txt)
				{
					for (String h:hyp)
					{
						if (t.equals(h))
							countMatch ++;
					}
				}
				System.out.println(countMatch + "," + txt.length +"," + hyp.length+";");
			}
			fr.close();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
