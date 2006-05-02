// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.*;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.part.ViewPart;

import net.sf.eclipsefp.haskell.ui.views.common.OpenViewerElement;
import net.sf.eclipsefp.haskell.ui.views.mbview.actions.ModuleBrowserGroup;


/** <p>The module browser is the main view on workspace resources from a
  * Haskell angle. It presents Haskell projects in a tree form.</p>
  * 
  * @author Leif Frenzel
  */
public class ModuleBrowser extends ViewPart implements IMenuListener {

  public static final String ID = ModuleBrowser.class.getName();

  private TreeViewer viewer;
  private UIState uiState;
  private Refresher refresher;
  private ModuleBrowserFilter filter = new ModuleBrowserFilter();
  private ModuleBrowserGroup actionGroup;
  private Menu contextMenu;
  

  public ISelectionProvider getSelectionProvider() {
    return viewer;
  }
  
  public ModuleBrowserFilter getFilter() {
    return filter;
  }

  public void refreshViewer() {
    viewer.getControl().setRedraw( false );
    viewer.refresh();
    viewer.getControl().setRedraw( true );
  }
  
  
  // overrriden methods of IViewPart
  //////////////////////////////////
  
  public void init( final IViewSite site, 
                    final IMemento memento ) throws PartInitException {
    super.init( site, memento );
    this.uiState = new UIState( this, memento );
    this.refresher = new Refresher( this );
  }
  
  public void saveState( final IMemento memento ) {
    uiState.saveState( memento );
  }
  
  public void dispose() {
    refresher.dispose();
    super.dispose();
  }
  
  
  // interface methods of ViewPart
  ////////////////////////////////
  
  public void createPartControl( final Composite parent ) {
    viewer = new TreeViewer( parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL );
    viewer.setUseHashlookup( true );
    
    initProviders();

    viewer.setInput( ResourcesPlugin.getWorkspace().getRoot() );
    viewer.addFilter( filter );
    viewer.setSorter( new ModuleBrowserTreeSorter() );

    uiState.applyTo( viewer );
    refresher.setViewer( viewer );
    
    MenuManager menuMgr = new MenuManager( "#PopupMenu" );
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( this );
    contextMenu = menuMgr.createContextMenu( viewer.getTree() );
    viewer.getTree().setMenu( contextMenu );
    
    // Register viewer with site. This must be done before making the actions.
    IWorkbenchPartSite site = getSite();
    site.registerContextMenu( menuMgr, viewer );
    site.setSelectionProvider( viewer );

    makeActions(); // must be called before registering for selection changes
    
    fillActionBars();
    hookDoubleClickAction();
  }

  public void setFocus() {
    if( viewer != null ) {
      viewer.getControl().setFocus();
    }
  }
  
  
  // interface methods of IMenuListener
  /////////////////////////////////////
  
  public void menuAboutToShow( final IMenuManager manager ) {
    // standard groups
    manager.add( new Separator( ModuleBrowserGroup.GROUP_NEW ) );
    manager.add( new Separator( ModuleBrowserGroup.GROUP_ADDITIONS ) );
    manager.add( new Separator( ModuleBrowserGroup.GROUP_PROPERTIES ) );
    
    actionGroup.setContext( new ActionContext( viewer.getSelection() ) );
    actionGroup.fillContextMenu( manager );
    actionGroup.setContext( null );
  }
  
  public IUIState getUIState() {
    return uiState;
  }

  public Object getAdapter( final Class key ) {
    Object result = null;
    if( key.equals( ISelectionProvider.class ) ) {
      result = viewer;
    } else {
      result = super.getAdapter( key );
    }
    return result;
  }
  
  
  // helping methods
  //////////////////

  private void initProviders() {
    viewer.setContentProvider( new ModuleBrowserContentProvider( uiState ) );
    viewer.setLabelProvider( new HaskellProjectLabelProvider( uiState ) );
  }
  
  private void makeActions() {
    actionGroup = new ModuleBrowserGroup( this );
  }
  
  private void fillActionBars() {
    IActionBars actionBars = getViewSite().getActionBars();
    actionGroup.fillActionBars( actionBars );
  }

  private void hookDoubleClickAction() {
    viewer.addDoubleClickListener( new IDoubleClickListener() {
      public void doubleClick( final DoubleClickEvent event ) {
        new OpenViewerElement( event ).run();
      }
    } );
  }
}