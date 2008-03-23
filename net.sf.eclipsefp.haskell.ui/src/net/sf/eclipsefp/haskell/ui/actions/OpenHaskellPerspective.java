// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.actions;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.WorkbenchException;


/** <p>An Action for opening the Haskell perspective.</p>
  *
  * @author Leif Frenzel
  */
public class OpenHaskellPerspective extends AbstractOpenAction {

  @Override
  public void run() {
    IWorkbenchWindow window = getWorkbench().getActiveWorkbenchWindow();
    IWorkbenchPage page = window.getActivePage();
    IAdaptable input
      = ( page != null ) ? page.getInput()
                         : ResourcesPlugin.getWorkspace().getRoot();
    try {
      String perspectiveId = HaskellUIPlugin.ID_PERSPECTIVE;
      getWorkbench().showPerspective( perspectiveId, window, input );
    } catch( WorkbenchException wbex ) {
      handleError( wbex,
                   "Could not open perspective.\nSee error log for details." );
    }
  }
}