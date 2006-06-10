// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors.outline;

import net.sf.eclipsefp.haskell.cabal.core.model.PackageDescription;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/** <p>the content provider for the tree on the Cabal file editor's 
  * outline page.</p>
  * 
  *  @author Leif Frenzel
  */
class CabalOutlinePageCP implements ITreeContentProvider {
  
  
  // interface methods of ITreeContentProvider
  ////////////////////////////////////////////

  public Object[] getElements( final Object inputElement ) {
    Object[] result = new Object[ 0 ];
    if( inputElement instanceof PackageDescription ) {
      PackageDescription pd = ( PackageDescription )inputElement;
      result = pd.getStanzas();
    }
    return result;
  }

  public boolean hasChildren( final Object element ) {
    return getChildren( element ).length > 0;
  }

  public Object[] getChildren( final Object parentElement ) {
    return new Object[ 0 ];
  }

  public Object getParent( final Object element ) {
    return null;
  }

  public void dispose() {
    // unused
  }

  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    // unused
  }
}