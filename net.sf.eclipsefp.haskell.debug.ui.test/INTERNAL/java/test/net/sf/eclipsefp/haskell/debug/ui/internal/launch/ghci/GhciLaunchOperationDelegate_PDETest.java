// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.ghci;

import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithProject;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/** <p>test cases for the launch operation delegate.</p>
  *
  * @author Leif Frenzel
  */
public class GhciLaunchOperationDelegate_PDETest extends TestCaseWithProject {

  public GhciLaunchOperationDelegate_PDETest() {
    addQualifier( GhcCompilerPlugin.getPluginId() );
  }

  public void testAddSourceFolders_single() throws Exception {
    // what we expect to show up in the command lines
    String locOfSrc = putInQuotes( project.getLocation().append( "src" ) );

    // source folders added by default
    HaskellProject hp = new HaskellProject( project );
    hp.addSourcePath( "src" );
    IFile[] files = new IFile[] { project.getFile( new Path( "src/Bla.hs" ) ) };

    GhciLaunchOperationDelegate del = new GhciLaunchOperationDelegate();
    assertContains( "-i" + locOfSrc, del.createArguments( hp, files ) );

    // unset the pref - no source folders
   // setPref( false );
   // assertContainsNot( "-i" + locOfSrc, del.createArguments( hp, files ) );

    // set the pref again - source folders again
  //  setPref( true );
    assertContains( "-i" + locOfSrc, del.createArguments( hp, files ) );
  }

  public void testAddSourceFolders_multi() throws Exception {
    // what we expect to show up in the command lines
    String locOfSrc = putInQuotes( project.getLocation().append( "src" ) );
    String locOfBlaSrc
      = putInQuotes( project.getLocation().append( new Path( "bla/src2" ) ) );

    HaskellProject hp = new HaskellProject( project );
    hp.addSourcePath( "src" );
    hp.addSourcePath( "bla/src2" );
    IFile[] files = new IFile[] { project.getFile( new Path( "src/Bla.hs" ) ) };

    GhciLaunchOperationDelegate del = new GhciLaunchOperationDelegate();
    assertContains( "-i" + locOfSrc, del.createArguments( hp, files ) );
    assertContains( "-i" + locOfBlaSrc, del.createArguments( hp, files ) );
  }

  public void testAddLinkedSourceFolder() throws Exception {
    IFolder folder = project.getFolder( "lsrc" );
    folder.createLink( new Path( "/bla/lsrc/" ), IResource.ALLOW_MISSING_LOCAL, null );

    HaskellProject hp = new HaskellProject( project );
    hp.addSourcePath( "lsrc" );
    IFile[] files = new IFile[] { project.getFile( new Path( "src/Bla.hs" ) ) };
    GhciLaunchOperationDelegate del = new GhciLaunchOperationDelegate();
    assertContains( "-i\"/bla/lsrc\"", del.createArguments( hp, files ) );
  }


  // helping methods
  //////////////////

  private String putInQuotes( final IPath path ) {
    return "\"" + path.toOSString() + "\"";
  }

  private void assertContains( final String candidate, final String[] args ) {
    boolean cntns = contains( candidate, args );
    assertTrue( candidate + " not in generated cmd line", cntns );
  }
/*
  private void assertContainsNot( final String candidate, final String[] args ) {
    boolean cntns = contains( candidate, args );
    assertFalse( candidate + " must not be in generated cmd line", cntns );
  }
*/
  private boolean contains( final String candidate, final String[] args ) {
    boolean result = false;
    for( String arg: args ) {
      result |= arg.equals( candidate );
    }
    return result;
  }

  /*private void setPref( final boolean value ) {
    IEclipsePreferences node = getPrefsScope().getNode( GhcCompilerPlugin.getPluginId() );
    node.putBoolean( IGhcPreferenceNames.GHCI_SOURCE_FOLDERS, value );
    try {
      node.flush();
    } catch( BackingStoreException ex ) {
      GhcCompilerPlugin.log( "Failed to store preferences", ex );
    }
  }*/
}
