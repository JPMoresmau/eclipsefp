// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.test.util.haskell;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

import junit.framework.TestCase;

/**
 * <p>
 * the super class for test cases that run on a Haskell project.
 * </p>
 * 
 * @author Leif Frenzel
 */
public abstract class HaskellProject_PDETestCase extends TestCase {

	private TestHaskellProject fProject;
	
	protected IProject getProject() {
		return fProject.getPlatformProject();
	}

	@Override
  protected final void setUp() throws Exception {
		fProject = new TestHaskellProject("TestProject-1");
		setUpMore();
	}

	/**
	 * Provide a point where subclasses can hook to add their own setUp behavior
	 * 
	 * @throws Exception
	 */
	protected void setUpMore() throws Exception {
	  // unused
	}

	/**
	 * Provide a point where subclasses can hook to add their own tearDown behavior
	 * 
	 * @throws Exception
	 */
	protected void doTearDown() throws Exception {
	  // unused
	}

	@Override
  protected final void tearDown() throws Exception {
		doTearDown();
		fProject.destroy();
	}

	protected IFile createSourceFile(final String contents, final String name)
			throws CoreException {
		return fProject.createSourceFile(name, contents);
	}
}
