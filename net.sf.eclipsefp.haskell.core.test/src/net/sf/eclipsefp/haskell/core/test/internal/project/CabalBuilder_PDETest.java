package net.sf.eclipsefp.haskell.core.test.internal.project;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;

public class CabalBuilder_PDETest extends TestCase {

  private static final String PROJECT_NAME = "p1";
  private IProject project;

  @Override
  protected void setUp() throws Exception {
    ProjectCreationOperation op = new ProjectCreationOperation();
    op.setProjectName( PROJECT_NAME );
    op.run( null );

    IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
    project = wsRoot.getProject( PROJECT_NAME );
  }

  @Override
  protected void tearDown() throws Exception {
    project.delete( true, null );
  }


  // actual test cases
  ////////////////////

  public void testNoCabalFileWarning() throws CoreException {
    assertTrue( project.exists() );
    assertFalse( project.getFile(  PROJECT_NAME + ".cabal" ).exists() );
    waitForAutoBuild();

    // no cabal file -> warning
    assertMarker( IMarker.SEVERITY_WARNING, "Cabal file" );
    // cabal file -> no warning
    IFile cabalFile = createCabalFile();
    waitForAutoBuild();
    assertEquals( 0, getAllMarkers().length );

    // again no cabal file -> warning
    cabalFile.delete( true, null );
    waitForAutoBuild();
    assertMarker( IMarker.SEVERITY_WARNING, "Cabal file" );
  }

  private IFile createCabalFile() throws CoreException {
    IFile result = project.getFile( PROJECT_NAME + ".cabal" );
    InputStream is = new ByteArrayInputStream( "".getBytes() );
    result.create( is, true, null );
    return result;
  }



  // helping functions
  ////////////////////

  // there is a marker on the project or one of its resources with the
  // specified severity and the message snipped is part or the marker message
  private boolean assertMarker( final int severity,
                                final String msg ) throws CoreException {
    IMarker[] markers = getAllMarkers();
    boolean result = false;
    for( IMarker marker: markers ) {
      Object sev = marker.getAttribute( IMarker.SEVERITY );
      if( sev instanceof Integer ) {
        Integer integer = ( Integer )sev;
        if( integer.intValue() == severity ) {
          String message = marker.getAttribute( IMarker.MESSAGE, "" );
          result |= message.indexOf( msg ) != -1;
        }
      }
    }
    return result;
  }

  private IMarker[] getAllMarkers() throws CoreException {
    String id = HaskellCorePlugin.ID_PROJECT_PROBLEM_MARKER;
    return project.findMarkers( id, true, IResource.DEPTH_INFINITE );
  }

  private void waitForAutoBuild() throws CoreException {
    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.build( IncrementalProjectBuilder.CLEAN_BUILD, null );
    System.out.print( "  Waiting for autobuild to complete ..." ); //$NON-NLS-1$
    IJobManager jobMan = Job.getJobManager();
    boolean retry = true;
    while( retry ) {
      try {
        jobMan.join( ResourcesPlugin.FAMILY_AUTO_REFRESH, null );
        jobMan.join( ResourcesPlugin.FAMILY_AUTO_BUILD, null );
        jobMan.join( ResourcesPlugin.FAMILY_MANUAL_BUILD, null );
        retry = false;
      } catch (Exception exc) {
        // ignore and retry
      }
    }
    System.out.print( " OK.\n" ); //$NON-NLS-1$
  }
}
