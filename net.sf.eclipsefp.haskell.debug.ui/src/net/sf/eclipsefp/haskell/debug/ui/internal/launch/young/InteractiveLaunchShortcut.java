package net.sf.eclipsefp.haskell.debug.ui.internal.launch.young;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.young.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.young.ILaunchAttributes;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;

/**
 * <p>
 * Shortcut to launch interactive sessions from the 'Run' action set.
 * </p>
 *
 * <p>
 * Subclasses must be declared in the <code>plugin.xml</code> and then need only
 * provide an implementation of <code>IInteractiveLaunchOperationDelegate</code>
 * that knows about specific details.
 * </p>
 *
 * @author Leif Frenzel
 * @author Alejandro Serrano
 * @deprecated
 */
public abstract class InteractiveLaunchShortcut implements ILaunchShortcut {

  /**
   * <p>
   * returns the delegate that knows about the specific details for launching an
   * interactive environment.
   * </p>
   */
  public abstract IInteractiveLaunchOperationDelegate getDelegate();


  // interface methods of ILaunchShortcut
  // /////////////////////////////////////

  public void launch( final ISelection selection, final String mode ) {
    // launched from workbench selection
    if( selection != null && selection instanceof IStructuredSelection ) {
      List<IResource> list = new ArrayList<IResource>();
      IStructuredSelection ssel = ( IStructuredSelection )selection;
      for( Object element: ssel.toList() ) {
        IResource res = ResourceUtil.findResource( element );
        if( res != null ) {
          list.add( res );
        }
      }
      IResource[] ress = ResourceUtil.toResourceArray( list );
      launch( ress, mode );
    }
  }


  public void launch( final IEditorPart editor, final String mode ) {
    // launched from editor part
    IResource resource = ResourceUtil.findResource( editor.getEditorInput() );
    launch( new IResource[] { resource }, mode );
  }


  // helping methods
  // ////////////////

  private void launch( final IResource[] resources, final String mode ) {
    // TODO put this into a Job and use the progress monitor
    // also: need a public job family in core (with icon in ui)
    IProgressMonitor monitor = new NullProgressMonitor();
    realLaunch( resources, mode, monitor );
    /*
     * new InteractiveLaunchOperation( getDelegate() ).launch( resources, mode,
     * monitor );
     */
  }

  private void realLaunch( final IResource[] resources, final String mode,
      final IProgressMonitor monitor ) {
    try {
      ArrayList<String> resourcePaths = new ArrayList<String>();
      for( IResource resource: resources ) {
        resourcePaths
            .add( resource.getProjectRelativePath().toPortableString() );
      }
      String configName = GhciInteractiveLaunchDelegate.class.getName();
      ILaunchConfigurationType type = LaunchOperation
          .getConfigType( configName );
      IProject project = resources[ 0 ].getProject();
      List<ILaunchConfiguration> launches = LaunchOperation
          .getConfigurationsForFiles( type, project.getName(), resourcePaths );
      IInteractiveLaunchOperationDelegate delegate = getDelegate();
      String id = LaunchOperation
          .createConfigId( project.getName()
              + ".interactive" //$NON-NLS-1$
              + " (" + resources[ 0 ].getProjectRelativePath().toPortableString() + ")" ); //$NON-NLS-1$ //$NON-NLS-2$
      ILaunchConfiguration launch = null;
      if( launches.size() > 0 ) {
        launch = launches.get( 0 );
      } else {
        // Create the launch configuration
        ILaunchConfigurationWorkingCopy wc = type.newInstance( null, id );
        String projectLocation = project.getLocation().toOSString();
        wc.setAttribute( ILaunchAttributes.INTERPRETER, delegate.getClass()
            .getName() );
        wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, projectLocation );
        wc.setAttribute( ILaunchAttributes.FILES, resourcePaths );
        wc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
        wc.setAttribute( ILaunchAttributes.PROJECT_NAME, project.getName() );
        wc.setAttribute( ILaunchAttributes.RELOAD_COMMAND,
            delegate.getReloadCommand() );
        // by default, reload when saving, that's why interactive sessions are
        // good
        wc.setAttribute( ILaunchAttributes.RELOAD, true );
        wc.doSave();
        launch = wc;
      }
      launch.launch( mode, monitor );
    } catch( CoreException e ) {
      // Nothing by now
    }
  }

}
