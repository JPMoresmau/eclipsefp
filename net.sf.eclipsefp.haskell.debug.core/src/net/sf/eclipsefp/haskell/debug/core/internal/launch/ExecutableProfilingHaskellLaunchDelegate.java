package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.io.File;
import java.util.Map;
import net.sf.eclipsefp.haskell.debug.core.internal.util.CoreTexts;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/**
 * launch delegate for Haskell executables executables
 *
 * @author JP Moresmau
 *
 */
public class ExecutableProfilingHaskellLaunchDelegate extends
    ExecutableOrTestSuiteHaskellLaunchDelegate {

  @Override
  protected String getExtraArguments() {
    return "+RTS -hT"; //$NON-NLS-1$
  }

  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch,
      final Map<String, String> processAttribs ) {
    // NOOP

  }

  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process ) {

    Job endJob = new Job( CoreTexts.testSuite_waiting ) {

      @Override
      protected IStatus run( final IProgressMonitor monitor ) {
        monitor.beginTask( CoreTexts.testSuite_waiting, 1 );
        while( !process.isTerminated() ) {
          if( monitor.isCanceled() ) {
            try {
              process.terminate();
            } catch( DebugException e ) {
              // Do nothing
            }
            return Status.CANCEL_STATUS;
          }
        }
        monitor.worked( 1 );

        try {
          IPath exeLocation = getExecutableLocation( configuration );
          IPath hpLocation = exeLocation.removeFileExtension()
              .addFileExtension( "hp" ); //$NON-NLS-1$
          final File fileToOpen = hpLocation.toFile();

          Display.getDefault().syncExec( new Runnable() {

            public void run() {
              try {
                if( fileToOpen.exists() && fileToOpen.isFile() ) {
                  IFileStore fileStore = EFS.getLocalFileSystem().getStore(
                      fileToOpen.toURI() );
                  IWorkbenchPage page = PlatformUI.getWorkbench()
                      .getActiveWorkbenchWindow().getActivePage();
                  IDE.openEditorOnFileStore( page, fileStore );
                } else {
                  // Do something if the file does not exist
                }
              } catch( Exception e ) {
                // Do nothing
              }
            }
          } );
        } catch( Exception e ) {
          return Status.CANCEL_STATUS;
        }

        return Status.OK_STATUS;
      }
    };
    endJob.schedule();
  }

}
