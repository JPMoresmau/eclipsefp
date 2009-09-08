// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;


/** <p>Visits the resource delta tree to incrementally build Haskell
  * projects.</p>
  *
  * @author Leif Frenzel
  */
class DeltaBuildVisitor extends Visitor implements IResourceDeltaVisitor {

  DeltaBuildVisitor(final IProgressMonitor monitor ) {
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
        String msg = "Error checking Haskell project nature."; //$NON-NLS-1$
        HaskellCorePlugin.log( msg, cex );
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
    if( file.exists() && (isHaskellFile( file ) || isCabalFile( file ))) {
      switch( delta.getKind() ) {
        case IResourceDelta.ADDED:
        case IResourceDelta.CHANGED:
          setNeedBuild( true );
          result = true;
          break;
        case IResourceDelta.REMOVED:
          setNeedBuild( true );
          result = true;
          break;
      }
    }
    return result;
  }
}