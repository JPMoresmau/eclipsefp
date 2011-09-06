package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;

/**
 * base class for launching executables and test-suites
 *
 * @author Alejandro Serrano
 *
 */
public abstract class ExecutableOrTestSuiteHaskellLaunchDelegate extends
    AbstractHaskellLaunchDelegate {

  protected abstract String getExtraArguments();

  @Override
  String[] determineArguments( final ILaunchConfiguration config )
      throws CoreException {
    String extra = config.getAttribute( ILaunchAttributes.EXTRA_ARGUMENTS,
        ILaunchAttributes.EMPTY );
    String args = config.getAttribute( ILaunchAttributes.ARGUMENTS,
        ILaunchAttributes.EMPTY );
    return CommandLineUtil.parse( extra + " " + args + " " + getExtraArguments() ); //$NON-NLS-1$ //$NON-NLS-2$
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
