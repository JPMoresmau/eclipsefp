// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.test;

import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;

/** <p>convenience super class for test cases that need a Haskell project
  * in the workspace.</p>
  *
  * @author Leif Frenzel
  */
public class TestCaseWithProject extends TestCase {

  protected static final String PROJECT_NAME = "p1";
  protected IProject project;

  @Override
  protected void setUp() throws Exception {
    ProjectCreationOperation op = new HaskellProjectCreationOperation();
    op.setProjectName( PROJECT_NAME );
    op.run( null );

    IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
    project = wsRoot.getProject( PROJECT_NAME );
  }

  @Override
  protected void tearDown() throws Exception {
    project.delete( true, null );
  }

}
