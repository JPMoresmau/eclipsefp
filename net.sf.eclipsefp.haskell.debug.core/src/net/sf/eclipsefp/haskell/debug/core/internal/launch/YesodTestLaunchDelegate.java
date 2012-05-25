/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;


/**
 * @author JP Moresmau
 *
 */
public class YesodTestLaunchDelegate extends YesodLaunchDelegate {

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.YesodLaunchDelegate#postProcessFinished()
   */
  @Override
  protected void postProcessFinished(final ILaunchConfiguration configuration) {
    try {
     IProject p=getProject( configuration );
     if (p!=null){
       // yesod at time of writing only write to dist
       IFolder fldr=p.getFolder( "dist" ); //$NON-NLS-1$
       if (fldr!=null){
         // refresh dist folder so that test result log is updated, etc
         fldr.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
       }
     }
    }catch (CoreException ce){
      HaskellDebugCore.log( ce.getLocalizedMessage(), ce );
    }
  }
}
