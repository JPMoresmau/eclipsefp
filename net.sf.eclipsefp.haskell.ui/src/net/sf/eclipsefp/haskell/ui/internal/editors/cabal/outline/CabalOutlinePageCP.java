// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.outline;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
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

  @Override
  public Object[] getElements( final Object inputElement ) {
    Object[] result = new Object[ 0 ];
    if( inputElement instanceof PackageDescription ) {
      PackageDescription pd = ( PackageDescription )inputElement;
      result = pd.getStanzas().toArray();
    }
    return result;
  }

  @Override
  public boolean hasChildren( final Object element ) {
    return getChildren( element ).length > 0;
  }

  @Override
  public Object[] getChildren( final Object parentElement ) {
    if (parentElement instanceof PackageDescriptionStanza){
      return ((PackageDescriptionStanza)parentElement).getStanzas().toArray();
    }
    return new Object[ 0 ];
  }

  @Override
  public Object getParent( final Object element ) {
    return null;
  }

  @Override
  public void dispose() {
    // unused
  }

  @Override
  public void inputChanged( final Viewer viewer,
                            final Object oldInput,
                            final Object newInput ) {
    // unused
  }
}