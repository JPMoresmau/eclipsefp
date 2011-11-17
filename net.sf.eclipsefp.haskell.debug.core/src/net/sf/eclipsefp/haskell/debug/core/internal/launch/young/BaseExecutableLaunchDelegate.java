/**
 *
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch.young;

import java.util.Map;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;


/**
 * @author Alejandro Serrano
 *
 */
public class BaseExecutableLaunchDelegate extends
    AbstractHaskellLaunchDelegate {

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate
   * #postProcessCreation(org.eclipse.debug.core.ILaunchConfiguration,
   * java.lang.String, org.eclipse.debug.core.ILaunch,
   * org.eclipse.debug.core.model.IProcess)
   */
  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process ) {
    // NOOP
  }

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate
   * #preProcessCreation(org.eclipse.debug.core.ILaunchConfiguration,
   * java.lang.String, org.eclipse.debug.core.ILaunch, java.util.Map)
   */
  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch,
      final Map<String, String> processAttribs ) {
    // NOOP
  }

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate
   * #preProcessDefinitionCreation(org.eclipse.debug.core.ILaunchConfiguration,
   * java.lang.String, org.eclipse.debug.core.ILaunch)
   */
  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode,
      final ILaunch launch ) {
    // NOOP
  }

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate#postProcessFinished()
   */
  @Override
  protected void postProcessFinished() {
    // NOOP
  }

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate
   * #getExecutableLocation(org.eclipse.debug.core.ILaunchConfiguration)
   */
  @Override
  public IPath getExecutableLocation( final ILaunchConfiguration config )
      throws CoreException {
    String projectName = config.getAttribute( ILaunchAttributes.PROJECT_NAME,
        ILaunchAttributes.EMPTY );
    String stanza = config.getAttribute( ILaunchAttributes.STANZA,
        ILaunchAttributes.EMPTY );
    IProject project = ResourcesPlugin.getWorkspace().getRoot()
        .getProject( projectName );
    return ResourceUtil.getExecutableLocation( project, stanza ).getRawLocation();
  }

  @Override
  protected IProject[] getBuildOrder( final ILaunchConfiguration configuration,
      final String mode ) throws CoreException {
    // indicate the project is to be looked at for unsaved files
    String prj = configuration.getAttribute( ILaunchAttributes.PROJECT_NAME,
        ( String )null );
    if( prj != null ) {
      IProject p = ResourcesPlugin.getWorkspace().getRoot().getProject( prj );
      if( p != null ) {
        return new IProject[] { p };
      }
    }
    return super.getBuildOrder( configuration, mode );
  }

}
