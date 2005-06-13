// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.views.mbview.actions;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.actions.NewWizardMenu;

import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;


/** <p>Action group that adds the 'New' menu to a context menu.</p>
  * 
  * @author Leif Frenzel
  */
public class NewWizardsActionGroup extends ActionGroup {

  private IWorkbenchSite site;
  
  public NewWizardsActionGroup( final IWorkbenchSite site ) {
    this.site = site;
  }
  

  // interface methods of ActionGroup
  ///////////////////////////////////
  
  public void fillContextMenu( final IMenuManager menu ) {
    super.fillContextMenu( menu );
    
    ISelection selection = getContext().getSelection();
    if( selection instanceof IStructuredSelection ) {
      IStructuredSelection sel = ( IStructuredSelection )selection;
      if( sel.size() <= 1 && isNewTarget( sel.getFirstElement() ) ) {
        IMenuManager newMenu = new MenuManager( "New" );
        menu.appendToGroup( ModuleBrowserGroup.GROUP_NEW, newMenu );
        new NewWizardMenu( newMenu, site.getWorkbenchWindow(), false );
      }
    }
  }

  
  // helping methods
  //////////////////
  
  private boolean isNewTarget( final Object element ) {
    boolean result = false;
    if( element == null ) {
      result = true;
    } else if ( element instanceof IResource ) {
      if( element instanceof IFile ) {
        if( !ResourceUtil.isProjectExecutable( ( IFile )element ) ) {
          result = true;
        }
      } else {
        result = true;
      }
    }
    return result;
  } 
}