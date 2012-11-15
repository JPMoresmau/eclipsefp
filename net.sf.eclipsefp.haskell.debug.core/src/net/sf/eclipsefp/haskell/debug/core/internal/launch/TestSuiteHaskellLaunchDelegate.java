package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.io.File;
import java.util.Map;
import java.util.Random;
import javax.xml.parsers.DocumentBuilderFactory;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.test.ITestListener;
import net.sf.eclipsefp.haskell.debug.core.test.TestListenerManager;
import net.sf.eclipsefp.haskell.debug.core.test.TestResult;
import net.sf.eclipsefp.haskell.debug.core.test.TestStatus;
import net.sf.eclipsefp.haskell.debug.core.test.TestSuite;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * launch delegate for test-framework test suites
 *
 * @author Alejandro Serrano
 * @author JP Moresmau
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
    return "--plain --jxml=\"" + getFilename() + "\" --jxml-nested"; //$NON-NLS-1$ //$NON-NLS-2$
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

  /**
   * Parse JUnit compatible XML element into a test result
   * @param et
   * @return
   */
  private TestResult parseTestResult(final Element et){
    String name=et.getAttribute( "name" ); //$NON-NLS-1$
    if (name!=null){
      TestResult ts=new TestResult(name);
      NodeList nl=et.getChildNodes();

      for (int a=0;a<nl.getLength();a++){
        if (nl.item( a ) instanceof Element){
          Element e=(Element)nl.item(a);
          if (e.getTagName().equals( "failure" )){//$NON-NLS-1$
            ts.setStatus( TestStatus.FAILURE );
            ts.setText( e.getTextContent() );
          } else if (e.getTagName().equals( "error" )){//$NON-NLS-1$
            ts.setStatus( TestStatus.ERROR );
            ts.setText( e.getTextContent() );
          } else {
            TestResult ctr=parseTestResult(e);
            if (ctr!=null){
              ts.getChildren().add( ctr );
            }
          }
        }
      }
      if (nl.getLength()==0){
        ts.setStatus( TestStatus.OK );
      }
      return ts;
    }
    return null;
  }

  @Override
  protected void postProcessFinished(final ILaunchConfiguration configuration) {
    // Get file and parse output
    final String fname = getFilename();
    final File file = new File( fname );

    try {
      Document d=DocumentBuilderFactory.newInstance().newDocumentBuilder().parse( file );

      TestResult root=parseTestResult( d.getDocumentElement() );
      TestSuite ts=new TestSuite( root );

      for (ITestListener l:TestListenerManager.getContributors().values()){
        l.start(ts);
        l.end( ts);
      }
    } catch (Throwable t){
      HaskellDebugCore.log( t.getLocalizedMessage(), t );
    } finally {
      // Always delete the file at the end
      file.delete();
    }

//    if (canShowJUnit()) {
//      Display.getDefault().syncExec( new Runnable() {
//
//        @Override
//        public void run() {
//          try {
//            IWorkbenchPage page = PlatformUI.getWorkbench()
//                .getActiveWorkbenchWindow().getActivePage();
//            page.showView( JUNIT_VIEW );
//            // JUnitCorePlugin.getModel().addTestRunSession( session );
//            JUnitModel.importTestRunSession( file );
//          } catch( CoreException e ) {
//            // Do nothing
//          }
//        }
//      } );
//    } else {
//      Display.getCurrent().syncExec( new Runnable() {
//
//        @Override
//        public void run() {
//          MessageDialog.openError( PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
//              CoreTexts.jdt_notFound_title, CoreTexts.jdt_notFound_message );
//        }
//      } );
//    }


  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#shouldContinue()
   */
  protected boolean canShowJUnit() {
    try {
      // Try to find JDT Core
      this.getClass().getClassLoader().loadClass( "org.eclipse.jdt.core.IJavaProject" ); //$NON-NLS-1$
      // Try to find JUnit model
      this.getClass().getClassLoader().loadClass( "org.eclipse.jdt.internal.junit.model.JUnitModel" ); //$NON-NLS-1$
      // Try to find JUnit view
      this.getClass().getClassLoader().loadClass( "org.eclipse.jdt.internal.junit.ui.TestRunnerViewPart" ); //$NON-NLS-1$
      // Everything is OK
      return true;
    } catch (Throwable t) {
      return false;
    }
  }

}
