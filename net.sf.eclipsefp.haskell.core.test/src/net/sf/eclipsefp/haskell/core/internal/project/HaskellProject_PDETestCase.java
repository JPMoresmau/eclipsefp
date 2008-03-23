// Copyright (c) 2004-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.internal.util.TestHaskellProject;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

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
