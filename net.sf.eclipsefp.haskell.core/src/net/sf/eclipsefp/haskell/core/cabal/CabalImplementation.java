package net.sf.eclipsefp.haskell.core.cabal;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
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
 * @author Scott Michel (scooter.phd@gmail.com)
 */
public class CabalImplementation {
  /** Base cabal executable name */
  public final static String CABAL_BASENAME = "cabal"; //$NON-NLS-1$
  /** Cabal's executable name (with ".exe" appended if running on Windows) */
  public final static String CABAL_EXECUTABLE = FileUtil.makeExecutableName( CABAL_BASENAME );

  /** Path to the cabal executable */
  private IPath fCabalExecutablePath;
  /** User identifier for this implementation */
  private String fCabalIdentifier;
  /** cabal-install version */
  private String fCabalInstallVersion;
  /** Cabal library version */
  private String fCabalLibraryVersion;

  /** Default constructor */
  public CabalImplementation() {
    fCabalIdentifier = null;
    fCabalExecutablePath = null;
    resetVersions();
  }

  /** Constructor with the cabal executable's Path */
  public CabalImplementation(final String ident, final IPath cabalExecPath) {
    fCabalIdentifier = ident;
    fCabalExecutablePath = cabalExecPath;
    probeVersionInternal(fCabalExecutablePath);
  }

  /** Copy constructor
   *
   *  @param src The source CabalImplementation object
   */
  public CabalImplementation (final CabalImplementation src) {
    this.copy(src);
  }

  /** Probe the version numbers of the "cabal" executable, in the given
   * directory.
   *
   * @param directory The directory in which to search for the cabal executable.
   */
  public void probeVersion(final String directory) {
    probeVersionInternal(new Path(directory).append( CABAL_EXECUTABLE ) );
  }

  /** Probe the version numbers of a proposed cabal executable, in the given
   * directory.
   *
   * @param directory The directory in which to search for the cabal executable.
   * @param exeName The name of the cabal executable.
   */
  public void probeVersion( final String directory, final String exeName ) {
    probeVersionInternal( FileUtil.makeExecutableName( new Path( directory ).append( exeName ) ));
  }

  /** Probe the version numbers of a cabal executable using the binary directory
   * of the Haskell implementation. Note that this defaults to using a cabal
   * executable name {@link #CABAL_EXECUTABLE}.
   *
   * @param hsImpl The Haskell implementation
   */
  public void probeVersion( final IHsImplementation hsImpl ) {
    if( hsImpl != null ) {
      IPath cabalBinPath = new Path( hsImpl.getBinDir() ).append( CABAL_EXECUTABLE );
      fCabalExecutablePath = cabalBinPath;
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
      String version = QueryUtil.queryEx( cabalExecutable.toOSString(), "--version" ); //$NON-NLS-1$

      if (version != null && version.length() > 0) {
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
  public IPath getCabalExecutableName() {
    return fCabalExecutablePath;
  }

  /** Set the operational cabal executable's name */
  public void setCabalExecutableName(final IPath executable) {
    fCabalExecutablePath = executable;
    probeVersionInternal( fCabalExecutablePath );
  }

  /** Reset the version strings to empty string */
  private void resetVersions() {
    fCabalInstallVersion = new String();
    fCabalLibraryVersion = new String();
  }

  /** Accessor for the user identifier */
  public final String getUserIdentifier () {
    return fCabalIdentifier;
  }

  /** Set the user identifier */
  public void setUserIdentifier (final String identifier) {
    fCabalIdentifier = identifier;
  }

  /** Accessor for cabal-install version string */
  public final String getInstallVersion() {
    return fCabalInstallVersion;
  }

  /** Set the cabal-install version string (hopefully, this was acquired
   * via probeVersion...)
   */
  public void setInstallVersion(final String installVersion) {
    fCabalInstallVersion = installVersion;
  }

  /** Accessor for Cabal library version string */
  public final String getLibraryVersion() {
    return fCabalLibraryVersion;
  }

  /** Set the Cabal library version string */
  public void setLibraryVersion(final String libVersion) {
    fCabalLibraryVersion = libVersion;
  }

  /** Copy method, used by the copy constructor and elsewhere */
  public void copy( final CabalImplementation theImpl ) {
    if (theImpl != null) {
      this.fCabalIdentifier = theImpl.fCabalIdentifier;
      this.fCabalExecutablePath = theImpl.fCabalExecutablePath;
      this.fCabalInstallVersion = theImpl.fCabalInstallVersion;
      this.fCabalLibraryVersion = theImpl.fCabalLibraryVersion;
    } else {
      fCabalIdentifier = null;
      fCabalExecutablePath = null;
      resetVersions();
    }
  }
}