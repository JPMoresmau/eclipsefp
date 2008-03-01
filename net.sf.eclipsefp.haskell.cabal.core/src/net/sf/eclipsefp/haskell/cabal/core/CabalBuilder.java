// Copyright (c) 2006-2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.core;

import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;

public class CabalBuilder extends IncrementalProjectBuilder {

  // interface methods of IncrementalProjectBuilder
  /////////////////////////////////////////////////

  @Override
  protected IProject[] build( final int kind,
                              final Map args,
                              final IProgressMonitor monitor )
                                                          throws CoreException {
    deleteAllMarkers();
    checkCabalFileExists();
    return null; // null means no deltas requested from other projects
  }


  // helping functions
  ////////////////////

  private void deleteAllMarkers() throws CoreException {
    String id = HaskellCorePlugin.ID_PROJECT_PROBLEM_MARKER;
    getProject().deleteMarkers( id, false, IResource.DEPTH_INFINITE );
  }

  private void checkCabalFileExists() throws CoreException {
    IFile cabalFile = getCabalFile();
    if( !cabalFile.exists() ) {
      String id = HaskellCorePlugin.ID_PROJECT_PROBLEM_MARKER;
      IMarker marker = getProject().createMarker( id );
      marker.setAttribute( IMarker.MESSAGE, "No cabal file in this project (or cabal file has a different name)" );
      marker.setAttribute( IMarker.SEVERITY, IMarker.SEVERITY_WARNING );
    }
  }

  private IFile getCabalFile() {
    String ext = ResourceUtil.EXTENSION_CABAL;
    IPath path = new Path( getProject().getName() ).addFileExtension( ext );
    return getProject().getFile( path );
  }
}
