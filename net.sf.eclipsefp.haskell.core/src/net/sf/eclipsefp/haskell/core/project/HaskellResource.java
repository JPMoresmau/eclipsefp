package net.sf.eclipsefp.haskell.core.project;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;

public class HaskellResource {

  private final IResource fResource;

  public HaskellResource( final IResource resource ) {
    fResource = resource;
  }

  public boolean isHaskellFile() {
    return    fResource instanceof IFile
           && ResourceUtil.hasHaskellExtension( fResource );
  }

  public boolean isProjectExecutable() {
    if( !( fResource instanceof IFile ) ) {
      return false;
    }
    IFile file = ( IFile )fResource;
    return    ResourceUtil.isInHaskellProject( file )
           && ResourceUtil.isProjectExecutable( file );
  }

  public boolean isSourceFolder() {
    if( !( fResource instanceof IFolder ) ) {
      return false;
    }
    IFolder folder = ( IFolder )fResource;
    return    ResourceUtil.isInHaskellProject( folder )
           && ResourceUtil.isSourceFolder( folder );
  }

}
