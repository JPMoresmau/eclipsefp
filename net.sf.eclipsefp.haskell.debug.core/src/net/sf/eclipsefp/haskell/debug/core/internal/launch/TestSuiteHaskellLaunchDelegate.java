package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.io.File;
import java.util.Map;
import java.util.Random;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jdt.junit.JUnitCore;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

/**
 * launch delegate for test-framework test suites
 *
 * @author Alejandro Serrano
 *
 */
public class TestSuiteHaskellLaunchDelegate extends
    ExecutableOrTestSuiteHaskellLaunchDelegate {

  static final String JUNIT_VIEW = "org.eclipse.jdt.junit.ResultView"; //$NON-NLS-1$

  String filename = null;

  private String getFilename() {
    if( filename == null ) {
      Random r = new Random( System.currentTimeMillis() );
      int n = r.nextInt();
      filename = HaskellDebugCore.getDefault().getStateLocation()
          .append( n + ".xml" ).toOSString(); //$NON-NLS-1$
    }
    return filename;
  }

  @Override
  protected String getExtraArguments() {
    return "--jxml=\"" + getFilename() + "\""; //$NON-NLS-1$ //$NON-NLS-2$
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
    // NOOP
  }

  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode, final ILaunch launch ) {
    // NOOP

  }

  @Override
  protected void postProcessFinished() {
    // Get file and parse output
    final String fname = getFilename();
    final File file = new File( fname );
    try {
      TestSuiteBeautifier.beuatify( file );
    } catch (Exception e) {
      // Do nothing
    }

    Display.getDefault().syncExec( new Runnable() {

      public void run() {
        try {
          IWorkbenchPage page = PlatformUI.getWorkbench()
              .getActiveWorkbenchWindow().getActivePage();
          page.showView( JUNIT_VIEW );
          JUnitCore.importTestRunSession( file );
        } catch( CoreException e ) {
          // Do nothing
        }
      }
    } );

    // Always delete the file at the end
    new File( getFilename() ).delete();
  }

}
