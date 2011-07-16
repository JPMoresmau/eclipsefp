package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.util.Map;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;

/**
 * launch delegate for Haskell executables executables
 * @author JP Moresmau
 *
 */
public class ExecutableHaskellLaunchDelegate extends
    AbstractHaskellLaunchDelegate {

  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final Map<String, String> processAttribs ) {
    // NOOP

  }

  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process )  {
    // NOOP

  }

  @Override
  protected IProject[] getBuildOrder( final ILaunchConfiguration configuration,
      final String mode ) throws CoreException {
    // indicate the project is to be looked at for unsaved files
    String prj=configuration.getAttribute( ILaunchAttributes.PROJECT_NAME, (String)null );
    if (prj!=null){
     IProject p=ResourcesPlugin.getWorkspace().getRoot().getProject( prj );
     if (p!=null){
       return new IProject[]{p};
     }
    }
    return super.getBuildOrder( configuration, mode );
  }

}
