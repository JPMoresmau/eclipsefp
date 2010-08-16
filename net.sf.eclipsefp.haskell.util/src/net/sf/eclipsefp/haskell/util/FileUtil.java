package net.sf.eclipsefp.haskell.util;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import org.eclipse.core.runtime.Path;

/**
 * Cross-platform file utilities.
 *
 * @author Thomas ten Cate (original)
 * @author Scott Michel (scottm@aero.org, modifications)
 */
public class FileUtil {
  static final ArrayList<File> candidateLocations = new ArrayList<File>(32);

  static {
    // Initialize the candidate file locations list, since this doesn't change during runtime.
    // add all directories from the PATH environment variable
    String path = System.getenv("PATH"); //$NON-NLS-1$
    for (String dir : path.split(File.pathSeparator)) {
      candidateLocations.add(new File(dir));
    }

    // add common bin directories from the user's home directory
    ArrayList<String> homes = new ArrayList<String>();
    final String envHome = System.getenv("HOME"); //$NON-NLS-1$
    
    homes.add(envHome);

    final String userHome = System.getProperty("user.home"); //$NON-NLS-1$
    if (!userHome.equals(envHome))
      homes.add(userHome);

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
	  if (PlatformUtil.runningOnWindows() && !baseName.endsWith( "." + PlatformUtil.WINDOWS_EXTENSION_EXE )) { //$NON-NLS-1$
	    return baseName + "." + PlatformUtil.WINDOWS_EXTENSION_EXE; //$NON-NLS-1$
	  }
	  return baseName;
	}

	/**
	 * Returns true if the given file is executable.
	 * On Unix, this checks the file permission bits.
	 * On Windows, it checks whether the file extension is that of an executable file.
	 */
	public static boolean isExecutable(final File file) {
		if (PlatformUtil.runningOnWindows()) {
		  return isExecutableWindows( file );
		}

		// Assume a UNIX flavor.
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
    for (String ext : PlatformUtil.WINDOWS_EXECUTABLE_EXTENSIONS) {
      if (fileExt.equals( ext )) {
        return true;
      }
    }
    return false;
  }

  public static File findExecutableInPath(final String shortFileName){
    return findInPath(makeExecutableName( shortFileName ),new FileFilter() {

      public boolean accept( final File pathname ) {
        return FileUtil.isExecutable( pathname );
      }
    });
  }

  public static File findInPath(final String shortFileName,final FileFilter ff){
    // Shallow copy is sufficient in this case
    ArrayList<File> candidates =new ArrayList<File>(candidateLocations);

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
