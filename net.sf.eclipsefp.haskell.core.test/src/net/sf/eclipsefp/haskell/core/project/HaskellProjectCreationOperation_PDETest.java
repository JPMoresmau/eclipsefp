// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperationPDETestCase;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectModelFilesOp;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

public class HaskellProjectCreationOperation_PDETest extends
		ProjectCreationOperationPDETestCase {

	@Override
  protected ProjectCreationOperation createOperation() {
    getCorePrefs().putBoolean( ICorePreferenceNames.FOLDERS_IN_NEW_PROJECT, true );
    HaskellProjectCreationOperation result
      = new HaskellProjectCreationOperation( );
    result.setExtraOperation( new ProjectModelFilesOp() );
    return result;
  }

  public void testAddsHaskellNature() throws InvocationTargetException,
      InterruptedException, CoreException {
    runOperation();

    IProject prj = getWorkspaceRoot().getProject( PROJECT_NAME );
    assertNotNull( prj.getNature( HaskellNature.NATURE_ID ) );
  }

	public void testCreatesDirectoriesFromPreferences()
      throws InvocationTargetException, InterruptedException {
	  getCorePrefs().put( ICorePreferenceNames.FOLDERS_SRC, "customSrc" );
	  getCorePrefs().put( ICorePreferenceNames.FOLDERS_OUT, "customOut" );
    getCorePrefs().put( ICorePreferenceNames.FOLDERS_BUILD, "customBuild" );

    runOperation();
    IProject prj = getWorkspaceRoot().getProject( PROJECT_NAME );

    assertValid( prj.getFolder( "customSrc" ) );
    assertValid( prj.getFolder( "customOut" ) );
    assertValid( prj.getFolder( "customBuild" ) );
  }

	public void testCreatesDescriptorFile() throws InvocationTargetException,
      InterruptedException {
    runOperation();
    IProject prj = getWorkspaceRoot().getProject( PROJECT_NAME );
    IFile f = prj.getFile( HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR );
    assertValid( f );

    assertValid( prj.getFile( prj.getName() + ".cabal" ) );
    assertValid( prj.getFile( "Setup.hs" ) );
  }

	public void testSetsUpProjectFoldersFromPreferences() throws Exception {
	  getCorePrefs().put( ICorePreferenceNames.FOLDERS_SRC, "mySrc" );
    getCorePrefs().put( ICorePreferenceNames.FOLDERS_OUT, "myOut" );
    getCorePrefs().put( ICorePreferenceNames.FOLDERS_BUILD, "myBuild" );
	  getCorePrefs().put( ICorePreferenceNames.TARGET_BINARY, "myBinary" );
	  getCorePrefs().put( ICorePreferenceNames.SELECTED_COMPILER, "null" );

    runOperation();
    // TODO TtC currently broken; create mock IHaskellProject that returns the right values
    IProject prj = getWorkspaceRoot().getProject( PROJECT_NAME );
    IFile f = prj.getFile( HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR );
    final String expectedContents = ""; // HaskellProjectManager.createDescriptorContent( "mySrc", "myOut", "myBin", "myBinary", "null" );
    assertEquals( expectedContents, readContents( f ) );
  }

  public void testDoNotCreateFoldersWhenPreferenceDisabled() throws Exception {
    getCorePrefs().putBoolean( ICorePreferenceNames.FOLDERS_IN_NEW_PROJECT, false );

    runOperation();

    IProject prj = getWorkspaceRoot().getProject( PROJECT_NAME );
    // should only contain the project descriptors (.project and .hsproject)
    // and the generated Setup.hs and cabal file
    assertEquals( 4, prj.members().length );

    IFile f = prj.getFile( HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR );
    assertValid( f );
    // Why would this be empty? -- TtC
    // assertEquals( "", readContents( f ) );
  }


	// helping methods
	//////////////////

	private String readContents( final IFile file ) throws Exception {
    StringBuffer buf = new StringBuffer( 1024 );
    InputStream input = file.getContents();
    BufferedReader reader = new BufferedReader( new InputStreamReader( input ) );
    String line;
    while( null != ( line = reader.readLine() ) ) {
      buf.append( line );
      buf.append( '\n' );
    }
    input.close();
    return buf.toString();
  }

  private void assertValid( final IResource res ) {
    assertNotNull( res );
    assertTrue( "Resource does not exist", res.exists() );
  }

}
