package net.sf.eclipsefp.haskell.core.cabal;

import java.io.File;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.sf.eclipsefp.haskell.core.internal.hsimpl.IHsImplementation;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.QueryUtil;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/**
 * Cabal implementation manager container.
 *
 * This class keeps track of various bits of important information related to
 * running Cabal. For example, ScionManager needs to know the current version
 * of the installed cabal in order to set installer flags.
 *
 * @author Scott Michel (scottm@aero.org)
 */
public class CabalImplementation {
  /** Cabal's executable name */
  public final static String CABAL_EXECUTABLE = FileUtil.makeExecutableName( "cabal" ); //$NON-NLS-1$

  /** cabal-install version */
  public String fCabalInstallVersion;
  /** Cabal library version */
  public String fCabalLibraryVersion;
  /** Path name to the cabal executable */
  public String fCabalExecutablePath;

  /** Default constructor */
  public CabalImplementation() {
    resetVersions();
    fCabalExecutablePath = new String();
  }

  /**
   * Query version identifiers from the cabal executable. Note that to do this requires that we have a
   * valid Haskell implementation that can clue us into the binary directory.
   *
   * NOTE: This method is expensive to call repeatedly because it involves forking a process to query
   * the CABAL_EXECUTABLE's version numbers, which then need to be parsed.
   */
  public void probeVersion(final IHsImplementation hsImpl) {
    boolean validImpl = false;

    if (hsImpl != null) {
      try {
        String version = QueryUtil.queryEx( getCabalExecutableName( hsImpl ), "--version" ); //$NON-NLS-1$
        String[] vlines = version.split( "(\\r\\n)|\\r|\\n" ); //$NON-NLS-1$

        if (vlines[0].startsWith( "cabal-install" ) //$NON-NLS-1$
            && vlines[1].startsWith( "using" ) ) { //$NON-NLS-1$
          // Looks like we might have a winner...
          Pattern vPat = Pattern.compile( "(\\d+\\.)+\\d+" ); //$NON-NLS-1$
          Matcher vMatch = vPat.matcher( vlines[0] );

          if (vMatch.find()) {
            fCabalInstallVersion = vMatch.group();
          }
          vMatch = vPat.matcher( vlines[1] );
          if (vMatch.find()) {
            fCabalLibraryVersion = vMatch.group();
          }

          if (fCabalInstallVersion.length() > 0 || fCabalLibraryVersion.length() > 0) {
            validImpl = true;
          }
        }
      } catch( IOException e ) {
        // Ignore, since validImpl will still be false.
      }
    }

    if (!validImpl) {
      resetVersions();
    }
  }

  /** Return the operational cabal executable name */
  public String getCabalExecutableName(final IHsImplementation hsImpl) {
    if (fCabalExecutablePath.length() != 0) {
      return fCabalExecutablePath;
    }

    boolean findInPath = false;

    if (hsImpl != null) {
      IPath cabalBinPath = new Path(hsImpl.getBinDir());
      cabalBinPath = cabalBinPath.append(CABAL_EXECUTABLE);

      File cabalBin = cabalBinPath.toFile();
      if ( !cabalBin.exists() ){
        findInPath = true;
      } else {
        try {
          fCabalExecutablePath = cabalBin.getCanonicalPath();
        } catch( IOException e ) {
          // Should never happen...
        }
      }
    }

    if ( findInPath ) {
      File cabalBin = FileUtil.findExecutableInPath( CABAL_EXECUTABLE );
      if (cabalBin.exists()) {
        try {
          fCabalExecutablePath = cabalBin.getCanonicalPath();
        } catch( IOException e ) {
          // Should never happen...
        }
      }
    }

    return fCabalExecutablePath;
  }
  /** Reset the version strings to empty string */
  private void resetVersions() {
    fCabalInstallVersion = new String();
    fCabalLibraryVersion = new String();
  }

  /** Accessor for cabal-install version string */
  public final String getInstallVersion() {
    return fCabalInstallVersion;
  }

  /** Accessor for Cabal library version string */
  public final String getLibraryVersion() {
    return fCabalLibraryVersion;
  }
}
