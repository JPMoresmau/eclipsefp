// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview.actions;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.actions.SelectionProviderAction;
import org.eclipse.ui.dialogs.ListSelectionDialog;

import net.sf.eclipsefp.haskell.ui.views.mbview.ModuleBrowser;
import net.sf.eclipsefp.haskell.ui.views.mbview.ModuleBrowserFilter;


/** <p>An action to open the filters dialog.</p>
  * 
  * @author Leif Frenzel
  */
class FilterSelectionAction extends SelectionProviderAction {
  
  private final ModuleBrowser moduleBrowser;
  
  public FilterSelectionAction( final ModuleBrowser moduleBrowser ) {
    super( moduleBrowser.getSelectionProvider(), "&Filters..." );
    this.moduleBrowser = moduleBrowser;
    setToolTipText( "Apply Filters" );
    setEnabled( true );
  }
  

  // interface methods of IAction
  ///////////////////////////////
  
  @Override
  public void run() {
    ModuleBrowserFilter filter = moduleBrowser.getFilter();
    String msg 
      = "Select the &filters to apply (matching elements will be hidden):";
    Object dummyInput = new Object();
    ListSelectionDialog dialog 
      = new ListSelectionDialog( moduleBrowser.getSite().getShell(), 
                                 dummyInput, 
                                 new FiltersContentProvider( filter ),
                                 new LabelProvider(), 
                                 msg );
    dialog.setTitle( "Module Browser Filters" );
    dialog.setInitialSelections( filter.getActiveCriteria() );
    dialog.open();
    if( dialog.getReturnCode() == Window.OK ) {
      Object[] results = dialog.getResult();
      filter.setActiveCriteria( results );
      moduleBrowser.refreshViewer();
    }
  }
}