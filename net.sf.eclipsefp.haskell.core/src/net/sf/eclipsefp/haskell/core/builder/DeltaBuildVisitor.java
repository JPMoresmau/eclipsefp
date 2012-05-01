// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
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

  @Override
  public boolean visit( final IResourceDelta delta ) {
    // we return whether to visit the children also
    boolean result = false;
    IResource res = delta.getResource();
    if( res instanceof IProject ) {
      IProject project = ( IProject )res;
       result = ResourceUtil.hasHaskellNature(project);
    } else if( res instanceof IFolder ) {
      result = !res.isDerived();
    } else if( res instanceof IFile ) {
      result = handleFileVisit( delta, ( IFile )res );
    }
    return result;
  }

  private boolean handleFileVisit( final IResourceDelta delta,
                                   final IFile file ) {
    boolean result = false;
    // && !file.isDerived() even if file is derived, it's been modified, hence we rebuild
    if( file.exists() && (FileUtil.hasHaskellExtension( file ) || FileUtil.hasCabalExtension( file )) ) {
      switch( delta.getKind() ) {
        case IResourceDelta.ADDED:
        case IResourceDelta.CHANGED:
          setNeedBuild( true );
          if (FileUtil.hasCabalExtension( file )){
            setNeedSynchronize( true);
            BuildWrapperPlugin.deleteProblems( file );
          }
          result = true;
          break;
        case IResourceDelta.REMOVED:
          setNeedBuild( true );
          setNeedSynchronize( FileUtil.hasCabalExtension( file ));
          result = true;
          break;
      }
    }
    return result;
  }
}