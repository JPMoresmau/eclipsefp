package net.sf.eclipsefp.haskell.core.project;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.hlint.HLintBuilder;
import net.sf.eclipsefp.haskell.core.partitioned.alex.AlexBuilder;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
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

  public boolean isProjectTestSuite(){
    if (fResource instanceof IProject){
      IProject project=(IProject)fResource;
      try {
        return !ResourceUtil.getProjectTestSuites( project ).isEmpty();
      } catch (CoreException ce){
        HaskellCorePlugin.log( ce );
      }
    }
    return false;
  }

  public boolean hasProjectHLintBuilder(){
    if (fResource instanceof IProject){
      IProject project=(IProject)fResource;
      try {
        IProjectDescription desc = project.getDescription();
        ICommand[] commands = desc.getBuildSpec();
        for( int i = 0; i < commands.length; ++i ) {
          if( commands[ i ].getBuilderName().equals( HLintBuilder.BUILDER_ID ) ) {
            return true;
          }
        }
      } catch (CoreException ce){
        HaskellCorePlugin.log( ce );
      }
    }
    return false;
  }

  public boolean needsProjectHLintBuilder(){
    return !hasProjectHLintBuilder();
  }

  public boolean hasProjectAlexBuilder(){
    if (fResource instanceof IProject){
      IProject project=(IProject)fResource;
      try {
        IProjectDescription desc = project.getDescription();
        ICommand[] commands = desc.getBuildSpec();
        for( int i = 0; i < commands.length; ++i ) {
          if( commands[ i ].getBuilderName().equals( AlexBuilder.BUILDER_ID ) ) {
            return true;
          }
        }
      } catch (CoreException ce){
        HaskellCorePlugin.log( ce );
      }
    }
    return false;
  }

  public boolean needsProjectAlexBuilder(){
    return !hasProjectAlexBuilder();
  }
}
