// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

import de.leiffrenzel.fp.haskell.core.project.ImportLibrariesList;

/** <p>the content provider for ImportLibraries on the viewer.</p>
  * 
  * @author Leif Frenzel
  */
class ImportLibrariesContentProvider implements IStructuredContentProvider {
  
  private ImportLibrariesList list;

  
  // interface methods of IStructuredContentProvider
  //////////////////////////////////////////////////
  
  public Object[] getElements( final Object input ) {
    return list == null ? new Object[ 0 ] : list.getAll();
  }
  
  public void dispose() {
    // unused
  }
  
  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    list = ( ImportLibrariesList )newInput;
  }
}