// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import net.sf.eclipsefp.haskell.core.project.ImportLibrariesList;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

/** <p>the content provider for ImportLibraries on the viewer.</p>
  *
  * @author Leif Frenzel
  * @deprecated
  */
class ImportLibrariesContentProvider implements IStructuredContentProvider {

  private ImportLibrariesList list;


  // interface methods of IStructuredContentProvider
  //////////////////////////////////////////////////

  @Override
  public Object[] getElements( final Object input ) {
    return list == null ? new Object[ 0 ] : list.getAll();
  }

  @Override
  public void dispose() {
    // unused
  }

  @Override
  public void inputChanged( final Viewer viewer,
                            final Object oldInput,
                            final Object newInput ) {
    list = ( ImportLibrariesList )newInput;
  }
}