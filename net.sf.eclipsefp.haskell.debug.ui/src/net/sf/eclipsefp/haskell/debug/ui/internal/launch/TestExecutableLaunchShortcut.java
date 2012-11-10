/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.Map;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;



/**
 * Launch a test-suite stanza as an executable
 * @author JP Moresmau
 *
 */
public class TestExecutableLaunchShortcut extends TestSuiteLaunchShortcut{

  @Override
  public IExecutableTestSuiteLaunchOperation getLaunchOperation() {
    return new ExecutableLaunchOperation(){
      /* (non-Javadoc)
       * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.ExecutableOrTestSuiteLaunchOperation#getExecutables(org.eclipse.core.resources.IProject)
       */
      @Override
      protected Map<String, IFile> getExecutables( final IProject project ) {
        return ResourceUtil.getProjectTestSuites( project );
      }
    };
  }
}
