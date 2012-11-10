package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.ILaunchShortcut2;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.eclipse.ui.navigator.ICommonMenuConstants;
import org.eclipse.ui.navigator.ICommonViewerSite;
import org.eclipse.ui.navigator.ICommonViewerWorkbenchSite;


public class RunActionProvider extends CommonActionProvider {

  private RunExecutableAction execAction;
  private RunProfilingAction profAction;
  private RunTestSuiteAction testAction;
  private RunTestExecutableAction testExecAction;

  @Override
  public void init( final ICommonActionExtensionSite aSite ) {
    super.init( aSite );
    ICommonViewerSite viewSite = aSite.getViewSite();
    if( viewSite instanceof ICommonViewerWorkbenchSite ) {
      ICommonViewerWorkbenchSite wSite = ( ICommonViewerWorkbenchSite )viewSite;
      execAction = new RunExecutableAction( wSite.getSelectionProvider() );
      profAction = new RunProfilingAction( wSite.getSelectionProvider() );
      testAction = new RunTestSuiteAction( wSite.getSelectionProvider() );
      testExecAction = new RunTestExecutableAction( wSite.getSelectionProvider() );
    }
  }

  @Override
  public void fillContextMenu( final IMenuManager menu ) {
    if( execAction != null && execAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, execAction );
    }
    if( profAction != null && profAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, profAction );
    }
    if( testAction != null && testAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, testAction );
    }
    if( testExecAction != null && testExecAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, testExecAction );
    }
  }

  private static class RunExecutableAction extends AbstractRunAction {

    private RunExecutableAction( final ISelectionProvider selProvider ) {
      super( UITexts.runExecutable, selProvider );
    }

    @Override
    protected CabalSyntax getTargetSection() {
      return CabalSyntax.SECTION_EXECUTABLE;
    }

    @Override
    protected ILaunchShortcut2 getShortcut() {
      return new ExecutableLaunchShortcut();
    }

//    @Override
//    protected String getLaunchConfigName() {
//      return BaseExecutableLaunchDelegate.class.getName();
//    }
//
//    @Override
//    protected ILaunchConfigurationWorkingCopy createLaunchConfig() throws CoreException {
//      ILaunchConfigurationType type = LaunchOperation
//          .getConfigType( getLaunchConfigName() );
//      String id = LaunchOperation.createConfigId( project.getName()
//          + "/" + stanza.getStanza().getName() ); //$NON-NLS-1$
//      ILaunchConfigurationWorkingCopy wc = type.newInstance( null, id );
//      wc.setAttribute( ILaunchAttributes.PROJECT_NAME, project.getName() );
//      wc.setAttribute( ILaunchAttributes.STANZA, stanza.getStanza().getName() );
//      wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, project
//          .getLocation().toOSString() );
//      wc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
//      return wc;
//    }

    @Override
    protected String getLaunchMode() {
      return ILaunchManager.RUN_MODE;
    }
  }

  private static class RunProfilingAction extends AbstractRunAction {

    private RunProfilingAction( final ISelectionProvider selProvider ) {
      super( UITexts.runProfiling, selProvider );
    }

    @Override
    protected CabalSyntax getTargetSection() {
      return CabalSyntax.SECTION_EXECUTABLE;
    }

    @Override
    protected ILaunchShortcut2 getShortcut() {
      return new ExecutableProfilingLaunchShortcut();
    }

//
//    @Override
//    protected String getLaunchConfigName() {
//      return ProfilingLaunchDelegate.class.getName();
//    }
//
//    @Override
//    protected ILaunchConfigurationWorkingCopy createLaunchConfig() throws CoreException {
//      ILaunchConfigurationType type = LaunchOperation
//          .getConfigType( getLaunchConfigName() );
//      String id = LaunchOperation.createConfigId( project.getName()
//          + "/" + stanza.getStanza().getName() ); //$NON-NLS-1$
//      ILaunchConfigurationWorkingCopy wc = type.newInstance( null, id );
//      wc.setAttribute( ILaunchAttributes.PROJECT_NAME, project.getName() );
//      wc.setAttribute( ILaunchAttributes.STANZA, stanza.getStanza().getName() );
//      wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, project
//          .getLocation().toOSString() );
//      wc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
//      return wc;
//    }

    @Override
    protected String getLaunchMode() {
      return ILaunchManager.RUN_MODE;
    }
  }

  private static class RunTestSuiteAction extends AbstractRunAction {

    private RunTestSuiteAction( final ISelectionProvider selProvider ) {
      super( UITexts.runTestFramework, selProvider );
    }

    @Override
    protected CabalSyntax getTargetSection() {
      return CabalSyntax.SECTION_TESTSUITE;
    }

    @Override
    protected ILaunchShortcut2 getShortcut() {
      return new TestSuiteLaunchShortcut();
    }

//    @Override
//    protected String getLaunchConfigName() {
//      return TestSuiteLaunchDelegate.class.getName();
//    }
//
//    @Override
//    protected ILaunchConfigurationWorkingCopy createLaunchConfig() throws CoreException {
//      ILaunchConfigurationType type = LaunchOperation
//          .getConfigType( getLaunchConfigName() );
//      String id = LaunchOperation.createConfigId( project.getName()
//          + "/" + stanza.getStanza().getName() ); //$NON-NLS-1$
//      ILaunchConfigurationWorkingCopy wc = type.newInstance( null, id );
//      wc.setAttribute( ILaunchAttributes.PROJECT_NAME, project.getName() );
//      wc.setAttribute( ILaunchAttributes.STANZA, stanza.getStanza().getName() );
//      wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, project
//          .getLocation().toOSString() );
//      wc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
//      return wc;
//    }

    @Override
    protected String getLaunchMode() {
      return ILaunchManager.RUN_MODE;
    }
  }

  private static class RunTestExecutableAction extends AbstractRunAction {

    private RunTestExecutableAction( final ISelectionProvider selProvider ) {
      super( UITexts.runTestSuite, selProvider );
    }

    @Override
    protected CabalSyntax getTargetSection() {
      return CabalSyntax.SECTION_TESTSUITE;
    }

    @Override
    protected ILaunchShortcut2 getShortcut() {
      return new TestExecutableLaunchShortcut();
    }

//    @Override
//    protected String getLaunchConfigName() {
//      return TestSuiteLaunchDelegate.class.getName();
//    }
//
//    @Override
//    protected ILaunchConfigurationWorkingCopy createLaunchConfig() throws CoreException {
//      ILaunchConfigurationType type = LaunchOperation
//          .getConfigType( getLaunchConfigName() );
//      String id = LaunchOperation.createConfigId( project.getName()
//          + "/" + stanza.getStanza().getName() ); //$NON-NLS-1$
//      ILaunchConfigurationWorkingCopy wc = type.newInstance( null, id );
//      wc.setAttribute( ILaunchAttributes.PROJECT_NAME, project.getName() );
//      wc.setAttribute( ILaunchAttributes.STANZA, stanza.getStanza().getName() );
//      wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, project
//          .getLocation().toOSString() );
//      wc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
//      return wc;
//    }

    @Override
    protected String getLaunchMode() {
      return ILaunchManager.RUN_MODE;
    }
  }
}
