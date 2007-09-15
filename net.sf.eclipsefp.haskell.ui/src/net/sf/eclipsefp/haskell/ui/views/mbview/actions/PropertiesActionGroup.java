// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview.actions;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.dialogs.PropertyDialogAction;


/** <p>Action group that adds the 'Properties' menu item to a context menu.</p>
  * 
  * @author Leif Frenzel
  */
class PropertiesActionGroup extends ActionGroup {

  private final PropertyDialogAction dlgOpenProperties;

  PropertiesActionGroup( final IWorkbenchSite site ) {
    ISelectionProvider selectionProvider = site.getSelectionProvider();
    dlgOpenProperties = new PropertyDialogAction( site, selectionProvider );

    // no need to register the open properties dialog action 
    // since it registers itself
    ISelectionProvider provider = site.getSelectionProvider();
    ISelection selection = provider.getSelection();
    if( selection instanceof IStructuredSelection ) {
      IStructuredSelection ssel = ( IStructuredSelection )selection;
      dlgOpenProperties.selectionChanged( ssel );
    } else {
      dlgOpenProperties.selectionChanged( selection );
    }
  }

  
  // interface methods of ActionGroup
  ///////////////////////////////////
  
  @Override
  public void fillActionBars( final IActionBars actionBar ) {
    super.fillActionBars( actionBar );
    setGlobalActionHandlers( actionBar );
  }
  
  @Override
  public void fillContextMenu( final IMenuManager menu ) {
    super.fillContextMenu( menu );
    IStructuredSelection selection = getStructuredSelection();
    if( dlgOpenProperties != null && dlgOpenProperties.isEnabled()
        && selection != null
        && dlgOpenProperties.isApplicableForSelection( selection ) ) {
      menu.appendToGroup( ModuleBrowserGroup.GROUP_PROPERTIES, 
                          dlgOpenProperties );
    }
  }

  
  // helping methods
  //////////////////
  
  private void setGlobalActionHandlers( final IActionBars actionBars ) {
    actionBars.setGlobalActionHandler( ActionFactory.PROPERTIES.getId(),
                                       dlgOpenProperties );
  }

  private IStructuredSelection getStructuredSelection() {
    IStructuredSelection result = null;
    ISelection selection = getContext().getSelection();
    if( selection instanceof IStructuredSelection ) {
      result = ( IStructuredSelection )selection;
    }
    return result;
  }
}