/**
 * Copyright (c) 2010, B. Scott Michel (scooter.phd@gmail.com)
 *
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabal;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
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
  private static final String NUM_INSTANCES = "numImpls"; //$NON-NLS-1$
  /** The default Cabal implementation's identifier preference */
  private static final String DEFAULT_CABAL_IMPLEMENTATION = "defaultImpl"; //$NON-NLS-1$
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

  /** The default constructor */
  private CabalImplementationManager() {
    // Load the implementations from the preference store:
    impls = deserializePrefs();

    String defaultImplIdent = getDefaultImplIdentPref();
    setDefaultCabalImplementation( defaultImplIdent, false );
  }

  /** Get the singleton instance */
  public static CabalImplementationManager getInstance() {
    return CabalImplsSingletonHolder.theInstance;
  }

  /** Get the default Cabal implementation */
  public final CabalImplementation getDefaultCabalImplementation() {
    return defaultImpl;
  }

  /** Set the default Cabal implementation, keyed by identiier */
  public final CabalImplementation setDefaultCabalImplementation(final String ident, final boolean syncPref) {
    defaultImpl = findImplementation( ident );
    if (syncPref) {
      IEclipsePreferences instanceNode = new InstanceScope().getNode( HaskellCorePlugin.getPluginId() );
      Preferences node = instanceNode.node( ICorePreferenceNames.CABAL_IMPLEMENTATIONS );
      try {
        node.put( DEFAULT_CABAL_IMPLEMENTATION, ident );
        node.flush();
      } catch (BackingStoreException ex) {
        HaskellCorePlugin.log( "Error setting default Cabal implementation preference", ex );
      }
    }
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
  public void setCabalImplementations( final List<CabalImplementation> impls ) {
    this.impls = impls;
    serializePrefs();
  }

  public void serializePrefs( ) {
    IEclipsePreferences instanceNode = new InstanceScope().getNode( HaskellCorePlugin.getPluginId() );

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

      node.flush();
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

  public List<CabalImplementation> deserializePrefs () {
    List<CabalImplementation> impls = new ArrayList<CabalImplementation>();
    IEclipsePreferences instanceNode = new InstanceScope().getNode( HaskellCorePlugin.getPluginId() );

    try {
      if (instanceNode.nodeExists( ICorePreferenceNames.CABAL_IMPLEMENTATIONS )) {
        Preferences node = instanceNode.node( ICorePreferenceNames.CABAL_IMPLEMENTATIONS);
        int numImpls = node.getInt( NUM_INSTANCES, 0);

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
              CabalImplementation impl = new CabalImplementation(ident, Path.fromPortableString( exePath ));
              if (   installVersion.equals(impl.getInstallVersion())
                  && libraryVersion.equals( impl.getLibraryVersion()) ) {
                impls.add(impl);
              } else {
                HaskellCorePlugin.log( "Install or library version mismatch", IStatus.ERROR ); //$NON-NLS-1$
              }
            } else {
              String diagnostic = "Invalid cabal implementation (".concat( String.valueOf(i) ).concat( "), missing items: "); //$NON-NLS-1$ //$NON-NLS-2$
              int diagLen = diagnostic.length();
              ArrayList<String> diags = new ArrayList<String>();
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
      }
    } catch (BackingStoreException ex) {
      // nothing.
    }

    return impls;
  }

  /** Get the default implementation's identifier from the preference hierarchy */
  private String getDefaultImplIdentPref() {
    String retval = new String();
    try {
      IEclipsePreferences instanceNode = new InstanceScope().getNode( HaskellCorePlugin.getPluginId() );
      if( instanceNode.nodeExists( ICorePreferenceNames.CABAL_IMPLEMENTATIONS ) ) {
        Preferences node = instanceNode.node( ICorePreferenceNames.CABAL_IMPLEMENTATIONS );
        String ident = node.get( DEFAULT_CABAL_IMPLEMENTATION, null );
        if( ident != null ) {
          retval = ident;
        }
      }
    } catch( BackingStoreException ex ) {
      // Ok, it doesn't appear to exist, so return an empty string
    }
    return retval;
  }
}