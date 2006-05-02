// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.actions;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.*;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;


/** <p>An Action for opening the Haskell perspective.</p>
  * 
  * @author Leif Frenzel
  */
public class OpenHaskellPerspective extends AbstractOpenAction {
  
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