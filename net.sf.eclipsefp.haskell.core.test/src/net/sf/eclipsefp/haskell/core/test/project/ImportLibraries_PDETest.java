// Copyright (c) 2007 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.test.project;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.IImportLibrary;
import net.sf.eclipsefp.haskell.core.project.ImportLibrariesList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;

/** <p>tests the creation of project-specific import libraries settings.</p> 
  *
  * @author Leif Frenzel
  */
public class ImportLibraries_PDETest extends TestCase {
	
	private IProject project;
	
	@Override
	protected void tearDown() throws Exception {
		if( project != null && project.exists() ) {
			project.delete( true, new NullProgressMonitor() );
		}
		super.tearDown();
	}
	
	// testing strategy:
	//	- assume we have a project in the workspace, and add an import library, 
	//	  then we expect a folder to show up in the project which is called 
	//	  .settings/ and contains a file net.sf.eclipsefp.haskell.core.prefs 
	//	  with a line that holds the import libs
	//	- if we add another import lib, it should also get into that file
	//	- if we delete the settings folder, we get no more import libs 
	//	  if we programmatically as for them
	public void testSettingsCreation() throws Exception {
		project = createProject();
		ImportLibrariesList list = new ImportLibrariesList( project );
		list.add( createLib( "foo" ) );
		list.save();
		
		IFolder folder = project.getFolder( ".settings" );
		assertTrue( folder.exists() );
		IFile file = folder.getFile( "net.sf.eclipsefp.haskell.core.prefs" );
		assertTrue( file.exists() );
		Properties props = loadProperties( file );
		String value = props.getProperty( "PROJECT_IMPORT_LIBRARIES" );
		assertTrue( value.contains( "foo" ) );
		
		list.add( createLib( "bar" ) );
		list.save();
		props = loadProperties( file );
		value = props.getProperty( "PROJECT_IMPORT_LIBRARIES" );
		assertTrue( value.contains( "bar" ) );

		folder.delete( true, new NullProgressMonitor() );
		list = new ImportLibrariesList( project );
        assertEquals( 0, list.getAll().length );
	}

	// testing strategy:
	//	- if we have an existing project with a .settings/ folder and an
	//	  appropriate content, we expect to find the import libs when that
	//	  project is opened in the first place
	public void testSettingsDigestion() throws Exception {
		project = createProject();
        IFolder folder = project.getFolder( ".settings" );
        folder.create( true, true, new NullProgressMonitor() );
        IFile file = folder.getFile( "net.sf.eclipsefp.haskell.core.prefs" );
        String content = "PROJECT_IMPORT_LIBRARIES=foo,t";
        InputStream bais = new ByteArrayInputStream( content.getBytes() );
        file.create( bais, true, new NullProgressMonitor() );
        
        ImportLibrariesList list = new ImportLibrariesList( project );
        assertEquals( 1, list.getAll().length );
        assertEquals( new Path( "foo" ), list.getAll()[ 0 ].getPath() );
        assertTrue( list.getAll()[ 0 ].isUsed() );
	}	
	
	
	// helping functions
	////////////////////
	
	private IProject createProject() throws CoreException {
		IWorkspace ws = ResourcesPlugin.getWorkspace();
		IProject result = ws.getRoot().getProject( "testProject" );
		IProjectDescription description = ws.newProjectDescription( "testProject" );
		description.setNatureIds( new String[] { HaskellNature.NATURE_ID } );
		result.create( description, new NullProgressMonitor() );
        result.open( new NullProgressMonitor() );
		return result;
	}

	private IImportLibrary createLib( final String path ) {
		return new IImportLibrary() {
			public IPath getPath() {
				return new Path( path );
			}

			public boolean isUsed() {
				return true;
			}

			public void setPath(IPath path) {
			}

			public void setUsed(boolean enabled) {
			}
		};
	}
	
	private Properties loadProperties(final IFile file) throws IOException,
			CoreException {
		Properties props = new Properties();
		InputStream is = file.getContents();
		props.load( is );
		is.close();
		return props;
	}
}
