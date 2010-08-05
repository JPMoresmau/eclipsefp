package net.sf.eclipsefp.haskell.core.util;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
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
  public static final String[] WINDOWS_EXECUTABLE_EXTENSIONS = new String[] { "exe", "bat" }; //$NON-NLS-1$ //$NON-NLS-2$
  static final ArrayList<File> candidateLocations = new ArrayList<File>(32);

  static {
    // Initialize the candidate file locations list, since this doesn't change during runtime.
    // add all directories from the PATH environment variable
    String path = System.getenv("PATH"); //$NON-NLS-1$
    for (String dir : path.split(File.pathSeparator)) {
      candidateLocations.add(new File(dir));
    }

    // add common bin directories from the user's home directory
    String[] homes = new String[] {
      System.getenv("HOME"), //$NON-NLS-1$
      System.getProperty("user.home") //$NON-NLS-1$
    };

    String[] userBins = new String[] {
      ".cabal/bin", //$NON-NLS-1$
      "usr/bin", //$NON-NLS-1$
      "bin", //$NON-NLS-1$
    };

    for (String home : homes) {
      for (String userBin : userBins) {
        candidateLocations.add(new File(home, userBin));
      }
    }
  }

	private FileUtil() {
		// do not instantiate
	}

	/**
	 * Makes the given base name into a valid executable file name.
	 * On Unix, this returns the base name itself.
	 * On Windows, it appends ".exe".
	 */
	public static String makeExecutableName(final String baseName) {
	  if (runningOnWindows() && !baseName.endsWith( "." + WINDOWS_EXECUTABLE_EXTENSIONS[0] )) { //$NON-NLS-1$
	    return baseName + "." + WINDOWS_EXECUTABLE_EXTENSIONS[0]; //$NON-NLS-1$
	  }
	  return baseName;
	}

	/**
	 * Returns true if the given file is executable.
	 * On Unix, this checks the file permission bits.
	 * On Windows, it checks whether the file extension is that of an executable file.
	 */
	public static boolean isExecutable(final File file) {
		if (runningOnWindows()) {
		  return isExecutableWindows( file );
		}

		// Assume a UNIX flavour.
  	return isExecutableUnix( file );
	}

  private static boolean isExecutableUnix( final File file ) {
    // Until Java 7, there is no way to check the executable bit of a file.
    // Apart from writing a JNI function (hassle), this is the best we can do...
    try {
    	Process process = Runtime.getRuntime().exec(new String[] {"test", "-x", file.getAbsolutePath()}); //$NON-NLS-1$ //$NON-NLS-2$
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

  public static File findExecutableInPath(final String shortFileName){
    return findInPath(makeExecutableName( shortFileName ),new FileFilter() {

      public boolean accept( final File pathname ) {
        return FileUtil.isExecutable( pathname );
      }
    });
  }

  public static File findInPath(final String shortFileName,final FileFilter ff){
    ArrayList<File> candidates = null;

    try {
      // Shallow copy is sufficient in this case
      @SuppressWarnings("unchecked")
      ArrayList<File> theClone = (ArrayList<File>) candidateLocations.clone();
      candidates = theClone;
    } catch (ClassCastException exc) {
      // But, in the really unexpected case when the cast actually
      // fails (unreachable code), do something sensible.
      candidates = new ArrayList<File>();
    }

    // Add the current working directory, since it might change.
    String pwd = System.getProperty("user.dir"); //$NON-NLS-1$
    candidates.add(new File(pwd));

    for (File candidate : candidates) {
      File file = new File(candidate, shortFileName);
      if (file.exists() && (ff==null || ff.accept( file ))) {
        return file;
      }
    }
    return null;
  }

  /**
   * Delete all contents including directories
   */
  static public boolean deleteRecursively( final File file ) {
    if ( file == null || !file.exists() ) {
      return true;
    }

    // If file is a file, delete it
    if ( file.isFile() ) {
      boolean del = file.delete();
      return del;
    }
    // The file is a directory
    File[] files = file.listFiles();
    for (File f:files){
      deleteRecursively(f);
    }
    boolean del = file.delete();
    return del;
  }
}
