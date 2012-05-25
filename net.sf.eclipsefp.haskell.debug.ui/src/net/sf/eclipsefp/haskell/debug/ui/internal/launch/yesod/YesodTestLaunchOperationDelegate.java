/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.yesod;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;


/**
 * Launch Operation Delegate for yesod test server
 * @author JP Moresmau
 *
 */
public class YesodTestLaunchOperationDelegate extends
    YesodDevelLaunchOperationDelegate {

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate#createArguments(org.eclipse.core.resources.IProject, org.eclipse.core.resources.IFile[])
   */
  @Override
  public String[] createArguments( final IProject hsProject, final IFile[] files,final String mode ) {
   return new String[]{"test"}; //$NON-NLS-1$
  }
}
