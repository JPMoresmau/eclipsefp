/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.yesod;

import net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;


/**
 * Launch Operation delegate for yesod development server
 * @author JP Moresmau
 *
 */
public class YesodDevelLaunchOperationDelegate implements
    IInteractiveLaunchOperationDelegate {

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate#getExecutable()
   */
  @Override
  public String getExecutable() {
    return ScionManager.getExecutablePath( IPreferenceConstants.YESOD_EXECUTABLE, "yesod",false ); //$NON-NLS-1$
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate#createArguments(org.eclipse.core.resources.IProject, org.eclipse.core.resources.IFile[])
   */
  @Override
  public String[] createArguments( final IProject hsProject, final IFile[] files,final String mode ) {
   return new String[]{"devel"}; //$NON-NLS-1$
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate#getReloadCommand()
   */
  @Override
  public String getReloadCommand() {
    return null;
  }


}
