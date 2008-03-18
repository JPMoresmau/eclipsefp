// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.test.internal.project;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.test.PDETestUtil;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithProject;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

public class CabalBuilder_PDETest extends TestCaseWithProject {

  public void testNoCabalFileWarning() throws CoreException {
    assertTrue( project.exists() );
    assertFalse( project.getFile(  PROJECT_NAME + ".cabal" ).exists() );
    PDETestUtil.waitForAutoBuild();

    // no cabal file -> warning
    assertMarker( IMarker.SEVERITY_WARNING, "No cabal file" );
    // cabal file -> no warning
    IFile cabalFile = createCabalFile();
    PDETestUtil.waitForAutoBuild();
    assertNoMarkerWithMsg( IMarker.SEVERITY_WARNING, "No cabal file" );

    // again no cabal file -> warning
    cabalFile.delete( true, null );
    PDETestUtil.waitForAutoBuild();
    assertMarker( IMarker.SEVERITY_WARNING, "No cabal file" );
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
  private void assertMarker( final int severity,
                             final String msg ) throws CoreException {
    List<IMarker> markers = getAllMarkers( severity );
    boolean result = false;
    for( IMarker marker: markers ) {
      String message = marker.getAttribute( IMarker.MESSAGE, "" );
      result |= message.indexOf( msg ) != -1;
    }
    assertTrue( "Expecting marker containing: " + msg, result );
  }

  private void assertNoMarkerWithMsg( final int severity,
                                      final String msg ) throws CoreException {
    List<IMarker> markers = getAllMarkers( severity );
    boolean result = true;
    for( IMarker marker: markers ) {
      String message = marker.getAttribute( IMarker.MESSAGE, "" );
      result &= message.indexOf( msg ) == -1;
    }
    assertTrue( "Must have no marker containing: " + msg, result );
  }

  private List<IMarker> getAllMarkers( final int severity ) throws CoreException {
    IMarker[] markers = getAllMarkers();
    List<IMarker> result = new ArrayList<IMarker>();
    for( IMarker marker: markers ) {
      Object sev = marker.getAttribute( IMarker.SEVERITY );
      if( sev instanceof Integer ) {
        Integer integer = ( Integer )sev;
        if( integer.intValue() == severity ) {
          result.add( marker );
        }
      }
    }
    return result;
  }

  private IMarker[] getAllMarkers() throws CoreException {
    String id = HaskellCorePlugin.ID_PROJECT_PROBLEM_MARKER;
    return project.findMarkers( id, true, IResource.DEPTH_INFINITE );
  }
}
