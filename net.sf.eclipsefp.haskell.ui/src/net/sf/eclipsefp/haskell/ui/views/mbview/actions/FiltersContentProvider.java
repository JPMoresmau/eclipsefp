// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview.actions;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

import net.sf.eclipsefp.haskell.ui.views.mbview.ModuleBrowserFilter;


/** <p>content provider for the pattern selection list on the filters 
  * dialog.</p>
  * 
  * @author Leif Frenzel
  */
class FiltersContentProvider implements IStructuredContentProvider {

  private ModuleBrowserFilter filter;
  
  FiltersContentProvider( final ModuleBrowserFilter filter ) {
    this.filter = filter;
  }
  

  // interface methods of IStructuredContentProvider
  //////////////////////////////////////////////////

  public Object[] getElements( final Object inputElement ) {
    return filter.getCriteria();
  }

  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    // unused
  }

  public void dispose() {
    // unused
  }
}