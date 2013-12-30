package net.sf.eclipsefp.haskell.core.project;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.hlint.HLintBuilder;
import net.sf.eclipsefp.haskell.core.partitioned.alex.AlexBuilder;
import net.sf.eclipsefp.haskell.core.partitioned.happy.HappyBuilder;
import net.sf.eclipsefp.haskell.core.partitioned.uuagc.UuagcBuilder;
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
       return !ResourceUtil.getProjectExecutables( project ).isEmpty();
    }
    return false;
  }

  public boolean isProjectYesod(){
    if (fResource instanceof IProject){
      IProject project=(IProject)fResource;
      if (project.isAccessible()){
        try {
          return project.hasNature( YesodNature.NATURE_ID );
        } catch (CoreException ce){
          HaskellCorePlugin.log( ce );
        }
      }
    }
    return false;
  }

  public boolean isProjectTestSuite(){
    if (fResource instanceof IProject){
      IProject project=(IProject)fResource;
       return !ResourceUtil.getProjectTestSuites( project ).isEmpty();
    }
    return false;
  }


  public boolean isProjectBenchmark(){
    if (fResource instanceof IProject){
      IProject project=(IProject)fResource;
       return !ResourceUtil.getProjectBenchmarks( project ).isEmpty();
    }
    return false;
  }

  public boolean hasProjectBuilder(final String builderId) {
    if (fResource instanceof IProject){
      IProject project=(IProject)fResource;
      try {
        IProjectDescription desc = project.getDescription();
        ICommand[] commands = desc.getBuildSpec();
        for( int i = 0; i < commands.length; ++i ) {
          if( commands[ i ].getBuilderName().equals( builderId ) ) {
            return true;
          }
        }
      } catch (CoreException ce){
        HaskellCorePlugin.log( ce );
      }
    }
    return false;
  }

  public boolean hasProjectHLintBuilder(){
    return hasProjectBuilder( HLintBuilder.BUILDER_ID );
  }

  public boolean needsProjectHLintBuilder(){
    return !hasProjectHLintBuilder();
  }

  public boolean hasProjectAlexBuilder(){
    return hasProjectBuilder( AlexBuilder.BUILDER_ID );
  }

  public boolean needsProjectAlexBuilder(){
    return !hasProjectAlexBuilder();
  }

  public boolean hasProjectHappyBuilder(){
    return hasProjectBuilder( HappyBuilder.BUILDER_ID );
  }

  public boolean needsProjectHappyBuilder(){
    return !hasProjectHappyBuilder();
  }

  public boolean hasProjectUuagcBuilder(){
    return hasProjectBuilder( UuagcBuilder.BUILDER_ID );
  }

  public boolean needsProjectUuagcBuilder(){
    return !hasProjectUuagcBuilder();
  }
}
