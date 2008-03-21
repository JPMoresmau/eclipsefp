// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.ghci;

import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithProject;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Preferences;

/** <p>test cases for the launch operation delegate.</p>
  *
  * @author Leif Frenzel
  */
public class GhciLaunchOperationDelegate_PDETest extends TestCaseWithProject {


  public void testAddSourceFolders_single() throws Exception {
    // source folders added by default
    HaskellProject hp = new HaskellProject( project );
    hp.addSourcePath( "src" );
    IFile[] files = new IFile[] { project.getFile( new Path( "src/Bla.hs" ) ) };

    GhciLaunchOperationDelegate del = new GhciLaunchOperationDelegate();
    assertContains( "-isrc", del.createArguments( hp, files ) );

    // unset the pref - no source folders
    setPref( false );
    assertContainsNot( "-isrc", del.createArguments( hp, files ) );

    // set the pref again - source folders again
    setPref( true );
    assertContains( "-isrc", del.createArguments( hp, files ) );
  }

  public void testAddSourceFolders_multi() throws Exception {
    HaskellProject hp = new HaskellProject( project );
    hp.addSourcePath( "src" );
    hp.addSourcePath( "bla/src2" );
    IFile[] files = new IFile[] { project.getFile( new Path( "src/Bla.hs" ) ) };

    GhciLaunchOperationDelegate del = new GhciLaunchOperationDelegate();
    assertContains( "-isrc", del.createArguments( hp, files ) );
    assertContains( "-ibla/src2", del.createArguments( hp, files ) );
  }


  // helping methods
  //////////////////

  private void assertContains( final String candidate, final String[] args ) {
    boolean cntns = contains( candidate, args );
    assertTrue( candidate + " not in generated cmd line", cntns );
  }

  private void assertContainsNot( final String candidate, final String[] args ) {
    boolean cntns = contains( candidate, args );
    assertFalse( candidate + " must not be in generated cmd line", cntns );
  }

  private boolean contains( final String candidate, final String[] args ) {
    boolean result = false;
    for( String arg: args ) {
      result |= arg.equals( candidate );
    }
    return result;
  }

  private void setPref( final boolean value ) {
    Preferences prefs = GhcCompilerPlugin.getDefault().getPluginPreferences();
    prefs.setValue( IGhcPreferenceNames.GHCI_SOURCE_FOLDERS, value );
    GhcCompilerPlugin.getDefault().savePluginPreferences();
  }
}
