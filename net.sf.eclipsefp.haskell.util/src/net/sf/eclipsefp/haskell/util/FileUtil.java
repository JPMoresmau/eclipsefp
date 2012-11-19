package net.sf.eclipsefp.haskell.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/**
 * Cross-platform file utilities and container for commonly used file and folder
 * names.
 * 
 * @author Thomas ten Cate (original)
 * @author Scott Michel (bscottm@ieee.org, modifications)
 */
public class FileUtil {
  /** Normal Haskell source extension. */
  public static final String   EXTENSION_HS       = "hs";                   //$NON-NLS-1$
  /** Literate Haskell (embedded TeX) source extension. */
  public static final String   EXTENSION_LHS      = "lhs";                  //$NON-NLS-1$
  /**  Haskell for C bindings source extension. */
  public static final String   EXTENSION_HSC      = "hsc";                  //$NON-NLS-1$
  /**
   * "Common Architecture for Building Applications and Libraries" (cabal)
   * project file source extension.
   */
  public static final String   EXTENSION_CABAL    = "cabal";                //$NON-NLS-1$

  /** Default project source directory */
  public static final String   DEFAULT_FOLDER_SRC = "src";
  /** Default project documentation directory */
  //public static final String   DEFAULT_FOLDER_DOC = "doc";
  
  /**  Haskell for Alex lexers. */
  public static final String   EXTENSION_ALEX     = "x";                  //$NON-NLS-1$
  /**  Haskell for Happy parsers. */
  public static final String   EXTENSION_HAPPY    = "y";                  //$NON-NLS-1$
  /**  Haskell for UU Attribute Grammars. */
  public static final String   EXTENSION_UUAGC    = "ag";                  //$NON-NLS-1$

  /**
   * UTF8 encoding
   */
  public static final String UTF8="UTF8"; //$NON-NLS-1$
  
  /** Candidate locations to search for files on a path */
  static final ArrayList<File> candidateLocations = new ArrayList<File>(32);

  
  public static Set<String> haskellExtensions=new HashSet<String>();
  
  static {
    // Initialize the candidate file locations list, since this doesn't
    // change during runtime.
    // add all directories from the PATH environment variable
    String path = System.getenv("PATH"); //$NON-NLS-1$
    for (String dir : path.split(File.pathSeparator)) {
      File f=new File(dir);
      candidateLocations.add(f);
      // the haskell platform doesn't always put these extra dirs in the path
      if (f.getParentFile()!=null && dir.contains("Haskell Platform")){
    	  candidateLocations.add(new File(f.getParentFile(),"lib/extralibs/bin"));
    	  candidateLocations.add(new File(f.getParentFile(),"mingw/bin"));
      }
    }

    // add common bin directories from the user's home directory
    ArrayList<String> homes = new ArrayList<String>();
    final String envHome = System.getenv("HOME"); //$NON-NLS-1$

    homes.add(envHome);

    final String userHome = System.getProperty("user.home"); //$NON-NLS-1$
    if (!userHome.equals(envHome))
      homes.add(userHome);

    String[] userBins = new String[] { ".cabal/bin", //$NON-NLS-1$
      "usr/bin", //$NON-NLS-1$
      "bin", //$NON-NLS-1$
      "AppData/Roaming/cabal/bin", // Windows 7 //$NON-NLS-1$
      "Application Data/cabal/bin" // Windows XP
    };

    for (String home : homes) {
      for (String userBin : userBins) {
        candidateLocations.add(new File(home, userBin));
      }
    }
    
    haskellExtensions.add(EXTENSION_HS);
    haskellExtensions.add(EXTENSION_LHS);
    haskellExtensions.add(EXTENSION_HSC);
  }

  private FileUtil() {
    // do not instantiate
  }

  /**
   * Makes the given base name into a valid executable file name. On Unix, this
   * returns the base name itself. On Windows, it appends ".exe".
   */
  public static String makeExecutableName(final String baseName) {
    if (PlatformUtil.runningOnWindows() && !baseName.endsWith("." + PlatformUtil.WINDOWS_EXTENSION_EXE)) { //$NON-NLS-1$
      return baseName + "." + PlatformUtil.WINDOWS_EXTENSION_EXE; //$NON-NLS-1$
    }
    return baseName;
  }
  
  /** Makes the given Path into a valid executable name, which is effectively a NOP on Unix, but ensures
   * that ".exe" is appended on Windows.
   */
  public static IPath makeExecutableName(final IPath exePath) {
    if (PlatformUtil.runningOnWindows()) {
      String ext = exePath.getFileExtension();
      if (ext == null || !ext.equals(PlatformUtil.WINDOWS_EXTENSION_EXE)) {
        return exePath.addFileExtension(PlatformUtil.WINDOWS_EXTENSION_EXE); 
      }
    }
    
    return exePath;
  }

  /**
   * Returns true if the given file is executable. On Unix, this checks the file
   * permission bits. On Windows, it checks whether the file extension is that
   * of an executable file.
   */
  public static boolean isExecutable(final File file) {
    if (PlatformUtil.runningOnWindows()) {
      return isExecutableWindows(file);
    }

    // Assume a UNIX flavor.
    return isExecutableUnix(file);
  }

  private static boolean isExecutableUnix(final File file) {
    // Until Java 7, there is no way to check the executable bit of a file.
    // Apart from writing a JNI function (hassle), this is the best we can
    // do...
    try {
      Process process = Runtime.getRuntime().exec(new String[] { "test", "-x", file.getAbsolutePath() }); //$NON-NLS-1$ //$NON-NLS-2$
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

  private static boolean isExecutableWindows(final File file) {
    String fileExt = new Path(file.getPath()).getFileExtension();
    for (String ext : PlatformUtil.WINDOWS_EXECUTABLE_EXTENSIONS) {
      if (fileExt.equals(ext)) {
        return true;
      }
    }
    return false;
  }

  public static File findExecutableInPath(final String shortFileName) {
    return findInPath(makeExecutableName(shortFileName), new FileFilter() {

      public boolean accept(final File pathname) {
        return FileUtil.isExecutable(pathname);
      }
    });
  }

  public static File findInPath(final String shortFileName, final FileFilter ff) {
    // Shallow copy is sufficient in this case
    ArrayList<File> candidates = new ArrayList<File>(candidateLocations);

    // Add the current working directory, since it might change.
    String pwd = System.getProperty("user.dir"); //$NON-NLS-1$
    candidates.add(new File(pwd));

    for (File candidate : candidates) {
      File file = new File(candidate, shortFileName);
      if (file.exists() && (ff == null || ff.accept(file))) {
        return file;
      }
    }
    return null;
  }
  

  /**
   * Delete all contents including directories
   */
  static public boolean deleteRecursively(final File file) {
    if (file == null || !file.exists()) {
      return true;
    }

    // If file is a file, delete it
    if (file.isFile()) {
      boolean del = file.delete();
      return del;
    }
    // The file is a directory
    File[] files = file.listFiles();
    for (File f : files) {
      deleteRecursively(f);
    }
    boolean del = file.delete();
    return del;
  }

  /**
   * <p>
   * returns whether the passed resource is a literate Haskell source file, as
   * recognized by the file extension '.lhs'.
   * </p>
   */
  public static boolean hasLiterateExtension(final IResource resource) {
    return has(resource, EXTENSION_LHS);
  }

  /**
   * Predicate that determines whether the resource is a Cabal project file.
   * 
   * @param resource
   *          The resource
   * @return true if the resource is a Cabal project file.
   */
  public static boolean hasCabalExtension(final IResource resource) {
    return has(resource, EXTENSION_CABAL);
  }

  /**
   * <p>
   * returns whether the passed resource is a Haskell source file, as recognized
   * by the file extensions '.hs' and '.lhs'.
   * </p>
   */
  public static boolean hasHaskellExtension(final IResource resource) {
    return has(resource, EXTENSION_HS) || has(resource, EXTENSION_LHS) || has(resource, EXTENSION_HSC);
  }
  
  public static boolean hasAnySourceExtension(final IResource resource) {
	  return hasHaskellExtension(resource) || hasCabalExtension(resource)
			 || has(resource,EXTENSION_ALEX)
			 || has(resource,EXTENSION_HAPPY)
			 || has(resource,EXTENSION_UUAGC);
  }
  

  private static boolean has(final IResource resource, final String extension) {
    if (resource != null) {
      String resExt = resource.getFileExtension();
      return resExt != null && resExt.equalsIgnoreCase(extension);
    }

    return false;
  }
  
  /** Return the list of candidate locations in which we search for various executables
   */
  public static final ArrayList<File> getCandidateLocations() {
    return candidateLocations;
  }
  
  public static String getContents(IFile f) throws Exception{
	  StringBuilder sb=new StringBuilder();
	  if (f.exists()){
		  ByteArrayOutputStream baos=new ByteArrayOutputStream();
		  InputStream is=f.getContents(true);
		  byte[] buf=new byte[4096];
		  int r=is.read(buf);
		  while (r>-1){
			  baos.write(buf,0,r);
			  r=is.read(buf);
		  }
		  is.close();
		  sb.append(new String(baos.toByteArray(),f.getCharset()));
	  }
	  return sb.toString();
  }
  
  public static String getContents(File f,String charSet) throws IOException{
	  StringBuilder sb=new StringBuilder();
	  if (f.exists()){
		  ByteArrayOutputStream baos=new ByteArrayOutputStream();
		  InputStream is=new BufferedInputStream(new FileInputStream(f));
		  byte[] buf=new byte[4096];
		  int r=is.read(buf);
		  while (r>-1){
			  baos.write(buf,0,r);
			  r=is.read(buf);
		  }
		  is.close();
		  sb.append(new String(baos.toByteArray(),charSet));
	  }
	  return sb.toString();
  }
  
  
  public static void writeSharedFile(File tgt,String contents,int tries) throws IOException{
	  try {
			Writer w=new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(tgt)),UTF8);
			w.write(contents);
			w.close();
	  } catch (FileNotFoundException e){
		  if (tries>1 && e.getMessage().toLowerCase(Locale.ENGLISH).contains("another process")){
			  try {
				  Thread.sleep(200);
			  } catch (InterruptedException ie){
				  // NOOP
			  }
			  writeSharedFile(tgt,contents,tries-1);
			  return;
		  }
		  throw e;
		} catch (IOException e){
			throw e;
		}
  }
}
