// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mdepview;

import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.part.DrillDownAdapter;
import org.eclipse.ui.part.ViewPart;

import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import net.sf.eclipsefp.haskell.ui.views.common.HaskellLabelProvider;
import net.sf.eclipsefp.haskell.ui.views.common.OpenViewerElement;


/** <p>A workbench view for module dependencies within a Haskell project.
  * Dependencies can be viewed in either direction: imports / imported by, 
  * in a hierarchical way.</p>
  *
  * @author Leif Frenzel 
  */
public class ModuleDependenciesView extends ViewPart {
  
  public static final String ID = ModuleDependenciesView.class.getName();
  
  private TreeViewer viewer;
  private DrillDownAdapter drillDownAdapter;
  private Action actionImports;
  private Action importedByAction;


  // interface methods of IViewPart
  /////////////////////////////////
  
  public void createPartControl( final Composite parent ) {
    // TODO if no dependencies have been selected yet, show a label with
    // a hint (like JDT type hierarchy)
    
    viewer = new TreeViewer( parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL );
    viewer.setContentProvider( new ModuleDependenciesContentProvider() );
    viewer.setLabelProvider( new HaskellLabelProvider() );
    viewer.setAutoExpandLevel( 2 );
    
    drillDownAdapter = new DrillDownAdapter( viewer );
    
    makeActions();
    hookContextMenu();
    hookDoubleClickAction();
    contributeToActionBars();
  }

  public void setFocus() {
    viewer.getControl().setFocus();
  }

  public void openTo( final Object element ) {
    viewer.setInput( element );
  }
  
  // ui control
  /////////////
  
  private void hookContextMenu() {
    MenuManager menuMgr = new MenuManager( "#PopupMenu" );
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( new IMenuListener() {
      public void menuAboutToShow( final IMenuManager manager ) {
        ModuleDependenciesView.this.fillContextMenu( manager );
      }
    } );
    Menu menu = menuMgr.createContextMenu( viewer.getControl() );
    viewer.getControl().setMenu( menu );
    getSite().registerContextMenu( menuMgr, viewer );
  }

  private void contributeToActionBars() {
    IActionBars bars = getViewSite().getActionBars();
    fillLocalPullDown( bars.getMenuManager() );
    fillLocalToolBar( bars.getToolBarManager() );
  }

  private void fillLocalPullDown( final IMenuManager manager ) {
    manager.add( actionImports );
    manager.add( new Separator() );
    manager.add( importedByAction );
  }

  private void fillContextMenu( final IMenuManager manager ) {
    manager.add( actionImports );
    manager.add( importedByAction );
    manager.add( new Separator() );
    drillDownAdapter.addNavigationActions( manager );
    // Other plug-ins can contribute there actions here
    manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
  }

  private void fillLocalToolBar( final IToolBarManager manager ) {
    manager.add( actionImports );
    manager.add( importedByAction );
    manager.add( new Separator() );
    drillDownAdapter.addNavigationActions( manager );
  }

  private void makeActions() {
    actionImports =  new Action() {
      public void run() {
        showMessage( "Imports" );
      }
    };
    
    configure( actionImports, 
               "Imports", 
               "Modules that are importe by this module.",
               IImageNames.DEP_VIEW_IMPORTS );

    importedByAction = new Action() {
      public void run() {
        showMessage( "Imported by - not yet implemented" );
      }
    };
    configure( importedByAction, 
               "Imported by", 
               "Modules that import this module.",
               IImageNames.DEP_VIEW_IMPORTEDBY );
  }

  private void configure( final Action action,
                          final String text, 
                          final String tooltipText,
                          final String imageKey ) {
    action.setText( text );
    action.setToolTipText( tooltipText );
    action.setImageDescriptor( HaskellUIImages.getImageDescriptor( imageKey ) );
  }

  private void hookDoubleClickAction() {
    viewer.addDoubleClickListener( new IDoubleClickListener() {
      public void doubleClick( final DoubleClickEvent event ) {
        new OpenViewerElement( event ).run();
      }
    } );
  }

  
  // helping methods
  //////////////////

  private void showMessage( final String message ) {
    MessageDialog.openInformation( viewer.getControl().getShell(),
        "Module Dependencies View", message );
  }
}