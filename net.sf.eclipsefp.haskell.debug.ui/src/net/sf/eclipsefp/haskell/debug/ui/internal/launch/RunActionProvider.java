/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
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

/**
 * Provider for contextual run actions on stanzas
 *
 * @author JP Moresmau
 *
 */
public class RunActionProvider extends CommonActionProvider {

  private RunExecutableAction execAction;
  private RunProfilingAction profAction;
  private RunTestSuiteAction testAction;
  private RunHTFAction htfAction;
  private RunTestExecutableAction testExecAction;
  private RunBenchmarkAction benchAction;

  @Override
  public void init( final ICommonActionExtensionSite aSite ) {
    super.init( aSite );
    ICommonViewerSite viewSite = aSite.getViewSite();
    if( viewSite instanceof ICommonViewerWorkbenchSite ) {
      ICommonViewerWorkbenchSite wSite = ( ICommonViewerWorkbenchSite )viewSite;
      ISelectionProvider selP=wSite.getSelectionProvider();
      execAction = new RunExecutableAction( selP );
      profAction = new RunProfilingAction( selP );
      htfAction = new RunHTFAction( selP );
      testAction = new RunTestSuiteAction( selP );
      testExecAction = new RunTestExecutableAction( selP );
      benchAction=new RunBenchmarkAction( selP );
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
    if( htfAction != null && htfAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, htfAction );
    }
    if( testAction != null && testAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, testAction );
    }
    if( testExecAction != null && testExecAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, testExecAction );
    }
    if (benchAction!=null && benchAction.isEnabled()){
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, benchAction );
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


    @Override
    protected String getLaunchMode() {
      return ILaunchManager.RUN_MODE;
    }
  }

  private static class RunHTFAction extends AbstractRunAction {

    private RunHTFAction( final ISelectionProvider selProvider ) {
      super( UITexts.runHTF, selProvider );
    }

    @Override
    protected CabalSyntax getTargetSection() {
      return CabalSyntax.SECTION_TESTSUITE;
    }

    @Override
    protected ILaunchShortcut2 getShortcut() {
      return new HTFLaunchShortcut();
    }


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


    @Override
    protected String getLaunchMode() {
      return ILaunchManager.RUN_MODE;
    }
  }

  private static class RunBenchmarkAction extends AbstractRunAction {

    private RunBenchmarkAction( final ISelectionProvider selProvider ) {
      super( UITexts.runBenchmark, selProvider );
    }

    @Override
    protected CabalSyntax getTargetSection() {
      return CabalSyntax.SECTION_BENCHMARK;
    }

    @Override
    protected ILaunchShortcut2 getShortcut() {
      return new BenchmarkLaunchShortcut();
    }


    @Override
    protected String getLaunchMode() {
      return ILaunchManager.RUN_MODE;
    }
  }
}
