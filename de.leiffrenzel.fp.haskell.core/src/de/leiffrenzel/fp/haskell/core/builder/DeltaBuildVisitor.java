// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.builder;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import de.leiffrenzel.fp.haskell.core.HaskellCorePlugin;
import de.leiffrenzel.fp.haskell.core.project.HaskellNature;


/** <p>Visits the resource delta tree to incrementally build Haskell 
  * projects.</p>
  * 
  * @author Leif Frenzel
  */
class DeltaBuildVisitor extends Visitor implements IResourceDeltaVisitor {
  
  DeltaBuildVisitor( final IProgressMonitor monitor ) {
    super( monitor );
  }

  public boolean visit( final IResourceDelta delta ) {
    // we return whether to visit the children also
    boolean result = false;
    IResource res = delta.getResource();
    if( res instanceof IProject ) {
      IProject project = ( IProject )res;
      try {
        result = project.hasNature( HaskellNature.NATURE_ID ); 
      } catch( CoreException cex ) {
        HaskellCorePlugin.log( "Error checking Haskell project nature.", cex );
        result = false;
      }
    } else if( res instanceof IFolder ) {
      result = true;
    } else if( res instanceof IFile ) {
      result = handleFileVisit( delta, ( IFile )res );
    }
    return result;
  }

  private boolean handleFileVisit( final IResourceDelta delta,
                                   final IFile file ) {
    boolean result = false;
    if( file.exists() && isHaskellFile( file ) ) {
      switch( delta.getKind() ) {
        case IResourceDelta.ADDED:
        case IResourceDelta.CHANGED:
          compileFile( file );
          result = true;
          break;
        case IResourceDelta.REMOVED:
          result = true;
          break;
      }
    } 
    return result;
  }
}