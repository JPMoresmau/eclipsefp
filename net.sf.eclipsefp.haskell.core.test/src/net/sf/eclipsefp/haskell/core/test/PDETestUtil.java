// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.test;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;

/** <p>collects some useful helping functionality for working with PDE
  * tests.</p>
  *
  * @author Leif Frenzel
  */
public class PDETestUtil {

  public static void waitForAutoBuild() throws CoreException {
    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.build( IncrementalProjectBuilder.CLEAN_BUILD, null );
    System.out.print( "  Waiting for autobuild to complete ..." ); //$NON-NLS-1$
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
    System.out.print( " OK.\n" ); //$NON-NLS-1$
  }

}
