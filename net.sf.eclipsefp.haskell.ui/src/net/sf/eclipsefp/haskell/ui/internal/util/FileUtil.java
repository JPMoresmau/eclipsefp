package net.sf.eclipsefp.haskell.ui.internal.util;

import java.io.File;
import java.io.IOException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;

/**
 * A class with some static helper methods to work with files.
 *
 * @author Thomas ten Cate
 */
public class FileUtil {

  /**
   * List of extensions of files that can be executed under Windows.
   * The first is the default in case we need to construct a valid executable file name.
   */
  public static final String[] WINDOWS_EXECUTABLE_EXTENSIONS = new String[] { "exe", "bat" };

	private FileUtil() {
		// do not instantiate
	}

	/**
	 * Makes the given base name into a valid executable file name.
	 * On Unix, this returns the base name itself.
	 * On Windows, it appends ".exe".
	 */
	public static String makeExecutableName(final String baseName) {
	  if (runningOnWindows()) {
	    return baseName + "." + WINDOWS_EXECUTABLE_EXTENSIONS[0];
	  } else {
	    return baseName;
	  }
	}

	/**
	 * Returns true if the given file is executable.
	 * On Unix, this checks the file permission bits.
	 * On Windows, it checks whether the file extension is that of an executable file.
	 */
	public static boolean isExecutable(final File file) {
		if (runningOnWindows()) {
		  return isExecutableWindows( file );
		} else {
		  // Assume a UNIX flavour.
  		return isExecutableUnix( file );
		}
	}

  private static boolean isExecutableUnix( final File file ) {
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
  }

  private static boolean isExecutableWindows( final File file ) {
    String fileExt = new Path(file.getPath()).getFileExtension();
    for (String ext : WINDOWS_EXECUTABLE_EXTENSIONS) {
      if (fileExt.equals( ext )) {
        return true;
      }
    }
    return false;
  }

  private static boolean runningOnWindows() {
    return Platform.getOS().equals( Platform.OS_WIN32 );
  }

}
