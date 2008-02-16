// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import java.util.Collection;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutputItem;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
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

  void compileFile( final IFile file ) {
	IHaskellProject hsProject = HaskellProjectManager.get(file.getProject());
	ICompilerOutput out = hsProject.compile(file);
    deleteMarkers( file );
    markResource( file, out.getErrors());
  }

  private void markResource( final IFile file,
                             final Collection<ICompilerOutputItem> items ) {
    try {
      for(ICompilerOutputItem item : items) {
        IMarker marker = file.createMarker( HaskellCorePlugin.ID_PROBLEM_MARKER );
        if( marker.exists() ) {
        	item.populateMarker(marker);
        }
      }
    } catch( CoreException cex ) {
      String msg = "Could not create markers for compiler output."; //$NON-NLS-1$
      HaskellCorePlugin.log( msg, cex );
    }
  }

  private void deleteMarkers( final IFile file ) {
    try {
      file.deleteMarkers( IMarker.PROBLEM, true, IResource.DEPTH_ZERO );
    } catch ( CoreException cex ) {
      HaskellCorePlugin.log( "Could not delete markers.", cex ); //$NON-NLS-1$
    }
  }
}