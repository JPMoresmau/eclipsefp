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
  private String fCabalInstallVersion;
  /** Cabal library version */
  private String fCabalLibraryVersion;
  /** Path name to the cabal executable */
  private String fCabalExecutablePath;

  /** Default constructor */
  public CabalImplementation() {
    resetVersions();
    fCabalExecutablePath = new String();
  }

  public CabalImplementation(final File cabalExec) {
    try {
      fCabalExecutablePath = cabalExec.getCanonicalPath();
      probeVersionInternal(new Path(fCabalExecutablePath));
    } catch (IOException e) {
      resetVersions();
      fCabalExecutablePath = new String();
    }
  }

  public void probeVersion(final String directoryHint) {
    probeVersionInternal(new Path(directoryHint, CABAL_EXECUTABLE));
  }
  public void probeVersion( final String directory, final String exeName ) {
    probeVersionInternal( new Path(directory, exeName) );
  }

  public void probeVersion( final IHsImplementation hsImpl ) {
    if( hsImpl != null ) {
      IPath cabalBinPath = new Path( hsImpl.getBinDir() );
      cabalBinPath = cabalBinPath.append( CABAL_EXECUTABLE );
      probeVersionInternal(cabalBinPath);
    }
  }

  /**
   * Query version identifiers from the cabal executable.
   *
   * NOTE: This method is expensive to call repeatedly because it involves forking a process to query
   * the CABAL_EXECUTABLE's version numbers, which then need to be parsed.
   */
  private void probeVersionInternal( final IPath cabalExecutable ) {
    boolean validImpl = false;

    try {
      String version = QueryUtil.queryEx( cabalExecutable.toOSString(),
          "--version" ); //$NON-NLS-1$
      String[] vlines = version.split( "(\\r\\n)|\\r|\\n" ); //$NON-NLS-1$

      if( vlines[ 0 ].startsWith( "cabal-install" ) //$NON-NLS-1$
          && vlines[ 1 ].startsWith( "using" ) ) { //$NON-NLS-1$
        // Looks like we might have a winner...
        Pattern vPat = Pattern.compile( "(\\d+\\.)+\\d+" ); //$NON-NLS-1$
        Matcher vMatch = vPat.matcher( vlines[ 0 ] );

        if( vMatch.find() ) {
          fCabalInstallVersion = vMatch.group();
        }
        vMatch = vPat.matcher( vlines[ 1 ] );
        if( vMatch.find() ) {
          fCabalLibraryVersion = vMatch.group();
        }

        if( fCabalInstallVersion.length() > 0
            || fCabalLibraryVersion.length() > 0 ) {
          validImpl = true;
        }
      }
    } catch( IOException e ) {
      // Paranoia: ensure validImpl is still false
      validImpl = false;
    }

    if( !validImpl ) {
      resetVersions();
    }
  }

  /** Return the operational cabal executable name */
  public String getCabalExecutableName() {
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
