import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.Vector;

public class SummerizeFile {

	/**
	 * @param args
	 */
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		String filePath = "/u/beltagy/workspace/deft/mln-semantics/data/bpLifted_trained_median_fixed_logodds_median";
		String outputFilePath= "/u/beltagy/workspace/deft/mln-semantics/data/bpLifted_trained_median_fixed_logodds_median_summery";
		
		try {
			BufferedReader fr = new BufferedReader(new FileReader(filePath));
			BufferedWriter fw = new BufferedWriter(new FileWriter(outputFilePath));
			String l;
			int section = 0; //0: any section
							//1: combination function
			String currentPair = "";
		
			while ((l = fr.readLine()) != null)
			{
				if(l.startsWith("  Pair"))
				{
					currentPair = l.substring(7);		
				}
				else if (l.startsWith("Command: "))
				{
					section = 1;
					
				}
				else if (l.startsWith("total time taken"))
				{
					section = 0;
					System.out.println(currentPair + ": " + l.split("=")[1]);
				}
				if (section == 0)
				{
					fw.write(l + "\n");
				}
			}
			fr.close();
			fw.close();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
