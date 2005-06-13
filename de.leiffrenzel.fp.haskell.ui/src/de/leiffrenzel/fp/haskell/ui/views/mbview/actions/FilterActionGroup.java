// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.views.mbview.actions;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.views.navigator.IResourceNavigator;

import de.leiffrenzel.fp.haskell.ui.util.HaskellUIImages;
import de.leiffrenzel.fp.haskell.ui.util.IImageNames;
import de.leiffrenzel.fp.haskell.ui.views.mbview.ModuleBrowser;


/** <p>action group for the 'Filters' entry in the view menu.</p>
  * 
  * @author Leif Frenzel
  */
class FilterActionGroup extends ActionGroup {

  protected IResourceNavigator navigator;
  
  private FilterSelectionAction filterAction;

  public FilterActionGroup( final ModuleBrowser moduleBrowser ) {
    filterAction = new FilterSelectionAction( moduleBrowser );
    String key = IImageNames.MB_VIEW_FILTER;
    ImageDescriptor imgDesc = HaskellUIImages.getImageDescriptor( key );
    filterAction.setImageDescriptor( imgDesc );
  }

  
  // interface methods of ActionGroup
  ///////////////////////////////////
  
  public void fillActionBars( final IActionBars actionBars ) {
    IMenuManager menu = actionBars.getMenuManager();
    menu.add( filterAction );
  }
}