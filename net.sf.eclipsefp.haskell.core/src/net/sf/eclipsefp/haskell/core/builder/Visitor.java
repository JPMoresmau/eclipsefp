// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;


/** <p>The superclass for visitors used in building Haskell projects</p>
  *
  * @author Leif Frenzel
  */
abstract class Visitor {

  private final IProgressMonitor monitor;

  Visitor( final IProgressMonitor monitor ) {
    this.monitor = monitor;
  }

  IProgressMonitor getMonitor() {
    return monitor;
  }

  boolean isHaskellFile( final IFile file ) {
    return ResourceUtil.hasHaskellExtension( file );
  }

  boolean isInSourceFolder( final IFile file ) {
    if( file == null || !file.isAccessible() ) {
      throw new IllegalArgumentException();
    }
    boolean result = false;
    IHaskellProject hsProject = HaskellProjectManager.get( file.getProject() );
    Set<IPath> sourcePaths = hsProject.getSourcePaths();
    for( IPath sourcePath: sourcePaths ) {
      IPath src = file.getProject().getFullPath().append( sourcePath );
      result |= src.isPrefixOf( file.getFullPath() );
    }
    return result;
  }

  void compileFile( final IFile file ) {
    IHaskellProject hsProject = HaskellProjectManager.get( file.getProject() );
    deleteMarkers( file );
    hsProject.compile( file );
  }

  private void deleteMarkers( final IFile file ) {
    try {
      file.deleteMarkers( IMarker.PROBLEM, true, IResource.DEPTH_ZERO );
    } catch ( CoreException cex ) {
      HaskellCorePlugin.log( "Could not delete markers.", cex ); //$NON-NLS-1$
    }
  }
}