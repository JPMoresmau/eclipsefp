// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview.actions;

import org.eclipse.jface.action.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;

import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import net.sf.eclipsefp.haskell.ui.views.mbview.IUIState;


/** <p>Groups the actions for toggling the Module Browser layout between
  * 'Flat' and 'Hierarchical'.</p>
  * 
  * @author Leif Frenzel
  */
class LayoutActionGroup extends MultiActionGroup {

  LayoutActionGroup( final IUIState uiState ) {
    int selectedState = uiState.isFlatLayout() ? 0 : 1;
    init( createActions( uiState ), selectedState );
  }

  
  // interface methods of IActionGroup
  ////////////////////////////////////
  
  public void fillActionBars( final IActionBars actionBars ) {
    super.fillActionBars( actionBars );
    contributeToViewMenu( actionBars.getMenuManager() );
  }
  
  
  // helpinfg methods
  ///////////////////
  
  private void contributeToViewMenu( final IMenuManager viewMenu ) {
    viewMenu.add( new Separator() );

    // Create layout sub menu
    IMenuManager layoutSubMenu = new MenuManager( "Layout" );
    final String layoutGroupName = "layout";
    Separator marker = new Separator( layoutGroupName );

    viewMenu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    viewMenu.add( marker );
    viewMenu.appendToGroup( layoutGroupName, layoutSubMenu );
    Separator endSep = new Separator(   IWorkbenchActionConstants.MB_ADDITIONS 
                                      + "-end" );
    viewMenu.add( endSep );    
    addActions( layoutSubMenu );
  }

  private IAction[] createActions( final IUIState uiState ) {
    IAction makeFlatLayoutAction = createAction( uiState,
                                                 true,
                                                 "Flat",
                                                 IImageNames.MB_VIEW_FLAT );
    
    String id = IImageNames.MB_VIEW_HIERARCHICAL;
    IAction makeHierarchicalLayoutAction = createAction( uiState, 
                                                         false,
                                                         "Hierarchical",
                                                         id );
    
    return new IAction[]{ makeFlatLayoutAction, 
                          makeHierarchicalLayoutAction };
  }
  
  private IAction createAction( final IUIState uiState, 
                                final boolean makeFlat, 
                                final String text, 
                                final String imageId ) {
    IAction action = new LayoutAction( uiState, makeFlat );
    action.setText( text );
    ImageDescriptor descr = HaskellUIImages.getImageDescriptor( imageId );
    action.setImageDescriptor( descr );
    return action;
  }

  
  // inner classes
  ////////////////
  
  /** action for toggling the flat/hierarchical layout */
  private class LayoutAction extends Action implements IAction {
  
    private IUIState uiState;
    /** whether this actions switches to flat layout or not. */
    private boolean makeFlat;
  
    public LayoutAction( final IUIState uiState, final boolean makeFlat ) {
      this.uiState = uiState;
      this.makeFlat = makeFlat;
    }
  
    
    // interface methods of IAction
    ///////////////////////////////
    
    public void run() {
      if( uiState.isFlatLayout() != makeFlat ) {
        uiState.toggleLayout();
      }
    }
  }
}