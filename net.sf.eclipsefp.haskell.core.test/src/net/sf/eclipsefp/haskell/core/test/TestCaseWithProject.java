// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.test;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectModelFilesOp;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;

/** <p>convenience super class for test cases that need a Haskell project
  * in the workspace.</p>
  *
  * @author Leif Frenzel
  */
public class TestCaseWithProject extends TestCaseWithPreferences {

  protected static final String PROJECT_NAME = "p1";
  protected IProject project;

  public TestCaseWithProject() {
    addQualifier( HaskellCorePlugin.getPluginId() );
  }

  public static void waitForAutoBuild() throws CoreException {
    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.build( IncrementalProjectBuilder.CLEAN_BUILD, null );
    // System.out.print( "  Waiting for autobuild to complete ..." ); //$NON-NLS-1$
    IJobManager jobMan = Job.getJobManager();
    boolean retry = true;
    while( retry ) {
      try {
        jobMan.join( ResourcesPlugin.FAMILY_AUTO_REFRESH, null );
        jobMan.join( ResourcesPlugin.FAMILY_AUTO_BUILD, null );
        jobMan.join( ResourcesPlugin.FAMILY_MANUAL_BUILD, null );
        retry = false;
      } catch (Exception exc) {
        // ignore and retry
      }
    }
    // System.out.print( " OK.\n" ); //$NON-NLS-1$
  }


  // interface methods of TestCase
  ////////////////////////////////

  @Override
  protected void setUp() throws Exception {
    super.setUp();

    ProjectCreationOperation op = new HaskellProjectCreationOperation();
    op.setExtraOperation( new ProjectModelFilesOp() );
    op.setProjectName( PROJECT_NAME );
    op.run( null );

    IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
    project = wsRoot.getProject( PROJECT_NAME );
  }

  @Override
  protected void tearDown() throws Exception {
    project.delete( true, null );
    super.tearDown();
  }
}
