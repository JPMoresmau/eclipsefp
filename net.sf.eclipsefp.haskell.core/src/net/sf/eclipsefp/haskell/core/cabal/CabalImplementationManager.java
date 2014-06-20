/**
 * Copyright (c) 2010, B. Scott Michel (bscottm@ieee.org)
 *
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabal;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.osgi.util.NLS;
import org.osgi.framework.Version;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

/** The container for managing Cabal implementations. This container also knows how to
 * serialize and de-serialize preferences as XML.
 *
 * @author B. Scott Michel
 *
 */
public class CabalImplementationManager {
  /** Number of instances serialized in preferences */
  public static final String NUM_INSTANCES = "numImpls"; //$NON-NLS-1$
  /** The default Cabal implementation's identifier preference */
  public static final String DEFAULT_CABAL_IMPLEMENTATION = "defaultImpl"; //$NON-NLS-1$
  /** Implementation's identifier preference node */
  private static final String ATT_NAME    = "name"; //$NON-NLS-1$
  /** XML attribute within {@link #ELEM_CABAL_IMPL} for the implementation's executable path */
  private static final String ATT_EXECUTABLE = "executable"; //$NON-NLS-1$
  /** XML attribute within {@link #ELEM_CABAL_IMPL} for the implementation's cabal-install version */
  private static final String ATT_INSTALL_VERSION = "installVersion"; //$NON-NLS-1$
  /** XML attribute within {@link #ELEM_CABAL_IMPL} for the implementation's Cabal library version */
  private static final String ATT_LIBRARY_VERSION = "libraryVersion"; //$NON-NLS-1$
  /** The array of Cabal implementations */
  private List<CabalImplementation> impls;
  /** The default implementation */
  private CabalImplementation defaultImpl;

  /** Singleton instance holder */
  private static class CabalImplsSingletonHolder {
    final static CabalImplementationManager theInstance = new CabalImplementationManager();
  }

  /** The default constructor, private to ensure that it remains a singleton instance. */
  private CabalImplementationManager() {
    // Load the implementations from the preference store:
    impls = new ArrayList<>();
    defaultImpl = null;
    deserializePrefs();

  }

  /** Get the singleton instance */
  public static CabalImplementationManager getInstance() {
    return CabalImplsSingletonHolder.theInstance;
  }

  /** Get the default Cabal implementation */
  public final CabalImplementation getDefaultCabalImplementation() {
    return defaultImpl;
  }

  public final CabalImplementation findImplementation( final String ident ) {
    CabalImplementation retval = null;
    for (CabalImplementation impl : impls) {
      if (impl.getUserIdentifier().equals( ident )) {
        retval = impl;
        break;
      }
    }
    return retval;
  }

  /** Get the implementations array, generally done when the UI's preference
   * pane is starting.
   */
  public List<CabalImplementation> getCabalImplementations() {
    return impls;
  }
  /** Set the implementations array, generally done when the UI's preference
   * pane has updated data.
   *
   * @param impls The newly updated implementations list
   */
  public void setCabalImplementations( final List<CabalImplementation> impls, final String defaultImplIdent ) {
    this.impls = impls;
    this.defaultImpl = findImplementation( defaultImplIdent );
    serializePrefs( defaultImplIdent );
  }

  public void serializePrefs( final String defaultImplIdent ) {
    IEclipsePreferences instanceNode = HaskellCorePlugin.instanceScopedPreferences();

    try {
      if (instanceNode.nodeExists( ICorePreferenceNames.CABAL_IMPLEMENTATIONS )) {
        instanceNode.node( ICorePreferenceNames.CABAL_IMPLEMENTATIONS).removeNode();
        instanceNode.remove( ICorePreferenceNames.CABAL_IMPLEMENTATIONS );
        instanceNode.flush();
      }

      Preferences node = instanceNode.node( ICorePreferenceNames.CABAL_IMPLEMENTATIONS );
      node.putInt( NUM_INSTANCES, impls.size() );

      int seqno = 1;
      for (CabalImplementation impl : impls) {
        Preferences implSeqNode = node.node( String.valueOf( seqno ) );
        serializePrefs( implSeqNode, impl );
        ++seqno;
      }

      node.put( DEFAULT_CABAL_IMPLEMENTATION, defaultImplIdent );
      node.sync();
      instanceNode.sync();
    } catch (BackingStoreException ex) {
      HaskellCorePlugin.log( ex.toString(), ex );
      try {
        // Remove the preferences.
        instanceNode.node(ICorePreferenceNames.CABAL_IMPLEMENTATIONS).removeNode();
        instanceNode.remove( ICorePreferenceNames.CABAL_IMPLEMENTATIONS );
        instanceNode.flush();
      } catch (BackingStoreException ex2) {
        // Ignore it.
      }
    }
  }

  private static void serializePrefs( final Preferences node, final CabalImplementation impl ) {
    node.put( ATT_NAME, impl.getUserIdentifier() );
    node.put( ATT_EXECUTABLE, impl.getCabalExecutableName().toPortableString());
    node.put( ATT_INSTALL_VERSION, impl.getInstallVersion() );
    node.put( ATT_LIBRARY_VERSION, impl.getLibraryVersion() );
  }

  public void deserializePrefs () {
    IEclipsePreferences instanceNode = HaskellCorePlugin.instanceScopedPreferences();

    try {
      if (instanceNode.nodeExists( ICorePreferenceNames.CABAL_IMPLEMENTATIONS )) {
        Preferences node = instanceNode.node( ICorePreferenceNames.CABAL_IMPLEMENTATIONS );
        int numImpls = node.getInt( NUM_INSTANCES, 0);
        String defaultImplIdent = node.get( DEFAULT_CABAL_IMPLEMENTATION, new String() );
        boolean shouldSave=false;
        for (int i = 1; i <= numImpls; ++i) {
          String implSeqString = String.valueOf(i);
          if (node.nodeExists( implSeqString )) {
            Preferences implNode = node.node( implSeqString );
            String ident = implNode.get( ATT_NAME, null );
            String exePath = implNode.get( ATT_EXECUTABLE, null);
            String installVersion = implNode.get(ATT_INSTALL_VERSION, null);
            String libraryVersion = implNode.get( ATT_LIBRARY_VERSION, null );

            if (   ident != null
                && exePath != null
                && installVersion != null
                && libraryVersion != null) {
              IPath path= Path.fromPortableString( exePath );
              CabalImplementation impl = new CabalImplementation(ident,path);
              File f=new File(path.toOSString());
              if (!f.exists()){
                shouldSave=true;
                HaskellCorePlugin.log( NLS.bind( CoreTexts.cabalexe_notfound, f.getAbsolutePath() ),  IStatus.ERROR );
              } else {
                impls.add(impl);

                if (!installVersion.equals(impl.getInstallVersion())){
                  shouldSave=true;
                  HaskellCorePlugin.log( NLS.bind( CoreTexts.cabalversion_mismatch_install, installVersion,impl.getInstallVersion() ),  IStatus.WARNING );
                }
                if (!libraryVersion.equals(impl.getLibraryVersion())){
                  shouldSave=true;
                  HaskellCorePlugin.log( NLS.bind( CoreTexts.cabalversion_mismatch_library, libraryVersion,impl.getLibraryVersion() ),  IStatus.WARNING );
                }
              }
            } else {
              String diagnostic = "Invalid cabal implementation (".concat( String.valueOf(i) ).concat( "), missing items: "); //$NON-NLS-1$ //$NON-NLS-2$
              int diagLen = diagnostic.length();
              ArrayList<String> diags = new ArrayList<>();
              if (ident == null) {
                diags.add("identifier"); //$NON-NLS-1$
              }
              if (exePath == null) {
                diags.add("executable path"); //$NON-NLS-1$
              }
              if (installVersion == null) {
                diags.add("install version"); //$NON-NLS-1$
              }
              if (libraryVersion == null) {
                diags.add("library version"); //$NON-NLS-1$
              }
              for (String diag : diags) {
                if (diagnostic.length() > diagLen) {
                  diagnostic.concat( ", " ); //$NON-NLS-1$
                }
                diagnostic.concat( diag );
              }

              HaskellCorePlugin.log( diagnostic, IStatus.ERROR );
            }
          }
        }

        defaultImpl = findImplementation( defaultImplIdent );

        if (shouldSave){
          serializePrefs( defaultImplIdent);
        }

      }
      if (impls.size()==0){
        List<CabalImplementation> detectedImpls=autoDetectCabalImpls();
        if (detectedImpls.size()>0){
          setCabalImplementations( detectedImpls, detectedImpls.get(0).getUserIdentifier() );
        }
      }
    } catch (BackingStoreException ex) {
      // nothing.
    }
  }

  public static List<CabalImplementation> autoDetectCabalImpls() {
    ArrayList<File> candidateLocs = FileUtil.getCandidateLocations();
    List<CabalImplementation> impls=new ArrayList<>();
    Set<String> paths=new HashSet<>();
    for (File loc : candidateLocs) {
      File[] files = loc.listFiles( new FilenameFilter() {
        @Override
        public boolean accept( final File dir, final String name ) {
          // Catch anything starting with "cabal", because MacPorts (and others) may install
          // "cabal-1.8.0" as a legitimate cabal executable.
          return name.startsWith( CabalImplementation.CABAL_BASENAME );
        }
      });

      if (files != null && files.length > 0) {
        for (File file : files) {
          try {
            String path=file.getCanonicalPath();
            if( paths.add( path )){
              CabalImplementation impl = new CabalImplementation("foo", new Path(path)); //$NON-NLS-1$
              // if we can't get a version, it's not a valid Cabal install
              if (impl.getInstallVersion()!=null && impl.getInstallVersion().length()>0){
                int seqno = 1;
                String ident = CabalImplementation.CABAL_BASENAME.concat( "-" ).concat(impl.getInstallVersion()); //$NON-NLS-1$
                if (!isUniqueUserIdentifier( ident, impls )) {
                  String uniqIdent = ident.concat( "-" ).concat( String.valueOf( seqno ) ); //$NON-NLS-1$
                  while (!isUniqueUserIdentifier(uniqIdent, impls)) {
                    seqno++;
                    uniqIdent = ident.concat( "-" ).concat( String.valueOf( seqno ) ); //$NON-NLS-1$
                  }
                  ident = uniqIdent;
                }

                impl.setUserIdentifier( ident );
                impls.add( impl );
              }
            }
          } catch (IOException e) {
            // Ignore?
          }
        }
      }
    }
    return impls;

  }

  public static boolean isUniqueUserIdentifier (final String ident,final List<CabalImplementation> impls) {
    boolean retval = true;
    for (CabalImplementation impl : impls) {
      if (impl.getUserIdentifier().equals(ident)) {
        retval = false;
        break;
      }
    }
    return retval;
  }

  public Preferences cabalImplementationsPreferenceNode() {
    IEclipsePreferences instanceNode = HaskellCorePlugin.instanceScopedPreferences();
    return instanceNode.node( ICorePreferenceNames.CABAL_IMPLEMENTATIONS );
  }

  public static String getCabalExecutable(){
    CabalImplementationManager cabalMgr = CabalImplementationManager.getInstance();
    CabalImplementation cabalImpl = cabalMgr.getDefaultCabalImplementation();
    if (cabalImpl!=null){
      final String cabalExecutable = cabalImpl.getCabalExecutableName().toOSString();
      if (cabalExecutable==null || cabalExecutable.length()<1){
        HaskellCorePlugin.log( CoreTexts.zerolenCabalExecutable_message,IStatus.ERROR);
        return null;
      }
      return cabalExecutable;
    }
    return null;
  }

  public static Version getCabalLibraryVersion(){
    CabalImplementationManager cabalMgr = CabalImplementationManager.getInstance();
    CabalImplementation cabalImpl = cabalMgr.getDefaultCabalImplementation();
    if (cabalImpl!=null){
      final String cabalVersion=cabalImpl.getLibraryVersion();
      if (cabalVersion!=null && cabalVersion.length()>0){
        return new Version(cabalVersion);
      }
    }
    return null;
  }
}