
package util;

import java.io.*;
import java.util.HashMap;

public class Word2Vec {

	public static void main(String[] args) throws IOException {

		String modelPath = "../../GoogleNews-vectors-negative300.bin";
		Word2Vec vec = new Word2Vec(modelPath);

		double[] man = vec.getVec ("man");
		double[] woman = vec.getVec ("woman");
		double[] water = vec.getVec ("water");
		System.out.println(man + " - "  + woman + " - " +  water);
		
	}
	private int words;
	public  int dimSize;
	private long offset = 0;
	private HashMap<String, Long> index = new HashMap<String, Long>();
	private RandomAccessFile modelFileReader = null;

	public Word2Vec (String modelPath) throws IOException
	{
		assert modelPath.endsWith(".bin") : "Invalid file name: " + modelPath;
		String indexPath = modelPath +  ".idx";
		File f = new File(indexPath);
		
		if(!f.exists() || f.isDirectory()) { 
			//Index file does not exist, so build it
			System.out.println("Indexing ... ");
			long start = System.currentTimeMillis();
			this.buildIndex(modelPath, indexPath) ;
			System.out.println("Done indexing .. " + (System.currentTimeMillis() - start)/1000 + "s" );
		}
		long start = System.currentTimeMillis();
		System.out.println("Reading index ... ");
		this.readIndex(indexPath);
		System.out.println("Done reading index .. " + (System.currentTimeMillis() - start)/1000 + "s" );
		this.modelFileReader = new RandomAccessFile(modelPath, "r");
	}

	public double [] getVec(String word) throws IOException {
		Long ptr = index.get(word);
		if (ptr != null && modelFileReader != null )
		{
			modelFileReader.seek(ptr);
			String modelWord = readString(modelFileReader);
			assert modelWord == word;
			double [] vector = readVector(modelFileReader, dimSize);
			return vector;
		}
		return null; //OOV
	}
	public void readIndex(String indexPath) throws IOException {
		BufferedReader fr = null;
		try {
			fr = new BufferedReader(new FileReader(indexPath));
			words = Integer.parseInt(fr.readLine());
			dimSize = Integer.parseInt(fr.readLine());
			String line = "";
			while((line = fr.readLine()) != null)
			{ 
				String[] splits = line.split(" ");
				assert splits.length == 2;
				String word = splits[0];
				long ptr = Long.parseLong(splits[1]);
				index.put(word, ptr);
			}
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		finally
		{
			fr.close();
		}

	}
	public void buildIndex(String modelPath, String indexPath) throws IOException {
		DataInputStream dis = null;
		BufferedInputStream bis = null;
		FileWriter fw = null;
		try {
			bis = new BufferedInputStream(new FileInputStream(modelPath));
			dis = new DataInputStream(bis);
			fw = new FileWriter(indexPath);
			offset = 0;
			words = Integer.parseInt(readString(dis));
			fw.write( words + "\n");
			dimSize = Integer.parseInt(readString(dis));
			fw.write(dimSize + "\n");	
			String word;
			long start  = System.currentTimeMillis();
			for (int i = 0; i < words; i++) {
				if (i*10 % words == 0)
					System.out.println(i*100 / words + "% .. " + (System.currentTimeMillis() - start)/1000 + "s") ;
				long ptr = offset;
				word = readString(dis);
				assert !word.contains(" ") : "Should not index words with spaces: " + word;
				fw.write( word + " " + ptr + "\n");
				readVector(dis, dimSize);
				//dis.read();
			}
		} 
		catch (Exception e)
		{
			e.printStackTrace();
		}
		finally {
			dis.close();
			fw.close();
		}
	}
	
	private String readString(DataInput dis) throws IOException {
		byte[] bytes = new byte[MAX_SIZE];
		byte b = dis.readByte();
		offset ++;
		int i = -1;
		StringBuilder sb = new StringBuilder();
		while (b != 32 && b != 10) {
			i++;
			bytes[i] = b;
			b = dis.readByte();
			offset ++;
			if (i == 49) {
				sb.append(new String(bytes));
				i = -1;
				bytes = new byte[MAX_SIZE];
			}
		}
		sb.append(new String(bytes, 0, i + 1));
		return sb.toString(); 
	}
	private static final int MAX_SIZE = 50;
	
	public double[] readVector(DataInput is, int size) throws IOException {
		double []vectors =  new double[size];
		double len = 0;
		float vector = 0;
		for (int j = 0; j < size; j++) {
			vector = readFloat(is);
			len += vector * vector;
			vectors[j] = (float) vector;
		}
		len = Math.sqrt(len);
		for (int j = 0; j < size; j++) {
			vectors[j] /= len;
		}
		return vectors;
	}
	public float readFloat(DataInput is) throws IOException {
		byte[] bytes = new byte[4];
		is.readFully(bytes);
		offset += 4;
		return getFloat(bytes);
	}
	public static float getFloat(byte[] b) {
		int accum = 0;
		accum = accum | (b[0] & 0xff) << 0;
		accum = accum | (b[1] & 0xff) << 8;
		accum = accum | (b[2] & 0xff) << 16;
		accum = accum | (b[3] & 0xff) << 24;
		return Float.intBitsToFloat(accum);
	}
}