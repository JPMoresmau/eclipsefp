// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;

/** <p>Shortcut to launch Haskell applications from the 'Run' action set.</p>
  *
  * @author Leif Frenzel
  */
public class ExecutableLaunchShortcut implements ILaunchShortcut {

  // interface methods of ILaunchShortcut
  ///////////////////////////////////////

  public void launch( final ISelection selection, final String mode ) {
    // launched from workbench selection
    if( selection instanceof IStructuredSelection ) {
      Object element = ( ( IStructuredSelection )selection ).getFirstElement();
      launch( ResourceUtil.findResource( element ) );
    }
  }

  public void launch( final IEditorPart editor, final String mode ) {
    // launched from editor part
    launch( ResourceUtil.findResource( editor.getEditorInput() ) );
  }


  // helping methods
  //////////////////

  private void launch( final IResource resource ) {
    // TODO put this in a Job and use the progress monitor
    try {
      new ExecutableLaunchOperation().launch( resource, null );
    } catch( CoreException cex ) {
      // TODO show msg box
      String msg = "Could not launch Haskell application."; //$NON-NLS-1$
      HaskellUIPlugin.log( msg, cex );
    }
  }
}