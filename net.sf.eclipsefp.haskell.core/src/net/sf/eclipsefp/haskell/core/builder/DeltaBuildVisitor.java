// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
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
    if( file.exists()){
      if (FileUtil.hasHaskellExtension( file ) || FileUtil.hasCabalExtension( file ) || isExtraHaskellFile(file)) {

        switch( delta.getKind() ) {

          case IResourceDelta.CHANGED:
            setNeedBuild( true );
            // cabal file changed, we need a synchronize
            if (FileUtil.hasCabalExtension( file )){
              setNeedSynchronize( true);
              BuildWrapperPlugin.deleteProblems( file );
              // file not modified by our editor: let's synchronize
            } else if (!HaskellCorePlugin.getModifiedByEditors().contains( file )){
              setNeedSynchronize( true);
            }
            result = true;
            break;
            // file added or removed, we need a synchronize
          case IResourceDelta.ADDED:
          case IResourceDelta.REMOVED:
            setNeedBuild( true );
            setNeedSynchronize( true); //FileUtil.hasCabalExtension( file )
            result = true;
            break;
        }
        /** project file has changed, maybe dependencies, etc, resynchronize **/
      } else if (file.getProjectRelativePath().toPortableString().equals(IProjectDescription.DESCRIPTION_FILE_NAME)){
        if (delta.getKind()==IResourceDelta.CHANGED){
          setNeedBuild( true );
          setNeedSynchronize( true);
          result = true;
        }
      }

    }
    return result;
  }

  private boolean isExtraHaskellFile(final IFile file){
    BWFacade bf=BuildWrapperPlugin.getFacade( file.getProject() );
    if (bf!=null){
      return ResourceUtil.getSourceContainer( file )!=null && !bf.isInTempFolder( file );
    }
    return false;
  }
}