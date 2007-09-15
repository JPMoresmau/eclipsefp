// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview.actions;

import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;
import org.eclipse.ui.actions.ActionGroup;

import net.sf.eclipsefp.haskell.ui.views.mbview.ModuleBrowser;


/** <p>contains all the actions for the Module Browser.</p>
  * 
  * @author Leif Frenzel
  */
public class ModuleBrowserGroup extends CompositeActionGroup 
                                implements ISelectionChangedListener {

  public static final String GROUP_NEW        = "GROUP_NEW";
  /** Note: must be named 'additions' for the workbench actions to hook in */
  public static final String GROUP_ADDITIONS  = "additions";
  public static final String GROUP_PROPERTIES = "GROUP_PROPERTIES";
  
  private final ModuleBrowser moduleBrowser;

  
  public ModuleBrowserGroup( final ModuleBrowser moduleBrowser ) {
    super();
    this.moduleBrowser = moduleBrowser;
    
    ISelectionProvider provider = getSite().getSelectionProvider();
    setGroups( new ActionGroup[] {
      // context menu
      new NewWizardsActionGroup( getSite() ),
      new PropertiesActionGroup( getSite() ),
      // view menu
      new FilterActionGroup( moduleBrowser ),
      new LayoutActionGroup( moduleBrowser.getUIState() )
    } );

    provider.addSelectionChangedListener( this );
  }

  @Override
  public void dispose() {
    ISelectionProvider provider = getSite().getSelectionProvider();
    provider.removeSelectionChangedListener( this );
    super.dispose();
  }
  
  
  // interface methods of ISelectionChangedListener
  /////////////////////////////////////////////////
  
  public void selectionChanged( final SelectionChangedEvent event ) {
    // unused
  }

  
  // action bars
  //////////////

  @Override
  public void fillActionBars( final IActionBars actionBars ) {
    super.fillActionBars( actionBars );
    fillViewMenu( actionBars.getMenuManager() );
  }

  private void fillViewMenu( final IMenuManager menu ) {
    addStandardAdditionsMenu( menu );
  }

  @Override
  public void fillContextMenu( final IMenuManager menu ) {
    addStandardAdditionsMenu( menu );
    // more, if necessary
    super.fillContextMenu( menu );
  }

  
  // helping methods
  //////////////////
  
  private void addStandardAdditionsMenu( final IMenuManager menu ) {
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    Separator endSep = new Separator(   IWorkbenchActionConstants.MB_ADDITIONS 
                                      + "-end" );
    menu.add( endSep );
  }
  
  private IWorkbenchPartSite getSite() {
    return moduleBrowser.getSite();
  }
}