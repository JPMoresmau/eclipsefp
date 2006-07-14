// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutputItem;
import net.sf.eclipsefp.haskell.core.compiler.IHaskellCompiler;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;


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
    return    file.getName().endsWith( ".hs" )
           || file.getName().endsWith( ".lhs" );    
  }
  
  void compileFile( final IFile file ) {
    // TODO should do sth. with the progress monitor
    CompilerManager man = CompilerManager.getInstance();
    // TODO refactor: move compiling code to hsProject itself
	IHaskellProject hsProject = HaskellProjectManager.get(file.getProject());
    IHaskellCompiler compiler = hsProject.getCompiler();
    ICompilerOutput out = compiler.compile( file );
    CompilerManager.getInstance().notifyListeners( out );
    deleteMarkers( file );
    if( man.hasParser() ) {
      ICompilerOutputItem[] items = man.getParser().getItems( out );
      markResource( file, items );
    } 
  }

  private void markResource( final IFile file, 
                             final ICompilerOutputItem[] items ) {
    try {
      for( int i = 0; i < items.length; i++ ) {
        IMarker marker = file.createMarker( HaskellCorePlugin.ID_PROBLEM_MARKER );
        if( marker.exists() ) {
        	items[i].populateMarker(marker);
        }
      }
    } catch( CoreException cex ) {
      HaskellCorePlugin.log( "Could not create markers for compiler output.", 
                             cex );
    }
  }
  
  private void deleteMarkers( final IFile file ) {
    try {
      file.deleteMarkers( IMarker.PROBLEM, true, IResource.DEPTH_ZERO );
    } catch ( CoreException cex ) {
      HaskellCorePlugin.log( "Could not delete markers.", cex );
    }    
  }
}