package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.io.File;
import java.util.Map;
import java.util.Random;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.internal.util.CoreTexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jdt.junit.JUnitCore;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

/**
 * launch delegate for Haskell executables executables
 * @author JP Moresmau
 *
 */
public class TestSuiteHaskellLaunchDelegate extends
    AbstractHaskellLaunchDelegate {

  String filename = null;

  private String getFilename() {
    if (filename == null) {
      Random r = new Random(System.currentTimeMillis());
      int n = r.nextInt();
      filename = HaskellDebugCore.getDefault().getStateLocation().append( n + ".xml" ).toOSString(); //$NON-NLS-1$
    }
    return filename;
  }

  @Override
  String[] determineArguments( final ILaunchConfiguration config )
      throws CoreException {
    String extra = config.getAttribute( ILaunchAttributes.EXTRA_ARGUMENTS,
        ILaunchAttributes.EMPTY );
    String args = config.getAttribute( ILaunchAttributes.ARGUMENTS,
        ILaunchAttributes.EMPTY );
    String xmlArg = "--jxml=\"" + getFilename() + "\"";  //$NON-NLS-1$ //$NON-NLS-2$
    return CommandLineUtil.parse( extra + " " + args + " " + xmlArg ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final Map<String, String> processAttribs ) {
    // NOOP

  }

  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process )  {
    // NOOP
    Job endJob = new Job(CoreTexts.testSuite_waiting) {
      @Override
      protected IStatus run( final IProgressMonitor monitor ) {
        monitor.beginTask( CoreTexts.testSuite_waiting, 1 );
        while (!process.isTerminated()) {
          if (monitor.isCanceled()) {
            try {
              process.terminate();
            } catch (DebugException e) {
              // Do nothing
            }
            return Status.CANCEL_STATUS;
          }
        }
        monitor.worked( 1 );

        // Get file and parse output
        final String fname = getFilename();
        Display.getDefault().syncExec( new Runnable() {
          public void run() {
            try {
              IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
              page.showView( "org.eclipse.jdt.junit.ResultView" ); //$NON-NLS-1$
              JUnitCore.importTestRunSession( new File( fname ) );
            } catch (CoreException e) {
              // Do nothing
            }
          }
        } );

        // Always delete the file at the end
        new File(getFilename()).delete();
        return Status.OK_STATUS;
      }
    };
    endJob.schedule();
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
