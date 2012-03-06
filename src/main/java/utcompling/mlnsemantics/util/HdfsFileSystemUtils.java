package utcompling.mlnsemantics.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileStatus;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;

public class HdfsFileSystemUtils {

	public static boolean delete(Path f, boolean recursive) throws IOException {
		boolean r = FileSystem.get(new Configuration()).delete(f, recursive);
		// System.out.println("Deleting " + f + ": " + r);
		return r;
	}

	public static boolean rename(Path src, Path dst) throws IOException {
		boolean r = FileSystem.get(new Configuration()).rename(src, dst);
		// System.out.println("Renaming " + src + " to " + dst + ": " + r);
		return r;
	}

	public static boolean mkdirs(Path f) throws IOException {
		boolean r = FileSystem.get(new Configuration()).mkdirs(f);
		// System.out.println("Making Dir " + f + ": " + r);
		return r;
	}

	public static boolean exists(Path f) throws IOException {
		return FileSystem.get(new Configuration()).exists(f);
	}

	public static boolean isDirectory(Path f) throws IOException {
		return FileSystem.get(new Configuration()).getFileStatus(f).isDir();
	}

	public static List<Path> listFiles(Path f) throws IOException {
		List<Path> files = new ArrayList<Path>();
		FileStatus[] statuses = FileSystem.get(new Configuration()).listStatus(
				f);
		for (FileStatus status : statuses) {
			if (!status.isDir()) {
				files.add(status.getPath());
			}
		}
		return files;
	}

	public static String readFile(Path path) throws IOException {
		BufferedReader f = null;
		try {
			FileSystem fs = FileSystem.get(new Configuration());
			f = new BufferedReader(new InputStreamReader(fs.open(path)));
			StringBuilder sb = new StringBuilder();
			for (String line = null; (line = f.readLine()) != null;)
				sb.append(line.trim() + "\n");
			return sb.toString();
		} finally {
			if (f != null)
				f.close();
		}
	}

	public static BufferedWriter openForWrite(Path f, boolean overwrite)
			throws IOException {
		return new BufferedWriter(new OutputStreamWriter(FileSystem.get(
				new Configuration()).create(f, overwrite)));
	}
}
