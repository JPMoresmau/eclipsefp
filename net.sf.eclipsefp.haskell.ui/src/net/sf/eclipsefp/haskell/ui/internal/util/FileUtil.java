package net.sf.eclipsefp.haskell.ui.internal.util;

import java.io.File;
import java.io.IOException;

/**
 * A class with some static helper methods to work with files.
 * 
 * @author Thomas ten Cate
 */
public class FileUtil {
	
	private FileUtil() {
		// do not instantiate
	}
	
	/**
	 * Returns true if the given file is executable.
	 * On Unix, this checks the file permission bits.
	 * On Windows, it checks whether the file extension is that of an executable file.
	 */
	public static boolean isExecutable(File file) {
		// if (we are on Unix) {
		// Until Java 7, there is no way to check the executable bit of a file.
		// Apart from writing a JNI function (hassle), this is the best we can do...
		try {
			Process process = Runtime.getRuntime().exec(new String[] {"test", "-x", file.getAbsolutePath()});
			int exitValue = process.waitFor();
			if (exitValue != 0) {
				return false;
			}
		} catch (IOException ex) {
			// pretend it succeeded
		} catch (InterruptedException ex) {
			// pretend it succeeded
		}
		return true;
		// } else if (we are on Windows) {
		// TODO implement this for Windows
		// }
	}

}
