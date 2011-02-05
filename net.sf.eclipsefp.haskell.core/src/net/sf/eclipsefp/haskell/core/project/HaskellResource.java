package net.sf.eclipsefp.haskell.core.project;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

public class HaskellResource {

  private final IResource fResource;

  public HaskellResource( final IResource resource ) {
    fResource = resource;
  }

  public boolean isHaskellFile() {
    return    fResource instanceof IFile
           && FileUtil.hasHaskellExtension( fResource );
  }

  public boolean isSourceFolder() {
    if( !( fResource instanceof IFolder ) ) {
      return false;
    }
    IFolder folder = ( IFolder )fResource;
    return    ResourceUtil.isInHaskellProject( folder )
           && ResourceUtil.isSourceFolder( folder );
  }

  public boolean isProjectExecutable(){
    if (fResource instanceof IProject){
      IProject project=(IProject)fResource;
      try {
        return !ResourceUtil.getProjectExecutables( project ).isEmpty();
      } catch (CoreException ce){
        HaskellCorePlugin.log( ce );
      }

    }
    return false;
  }
}
