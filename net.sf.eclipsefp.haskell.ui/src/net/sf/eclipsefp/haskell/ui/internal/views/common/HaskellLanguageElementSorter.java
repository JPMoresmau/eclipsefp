// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.views.common;

import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

/** <p>sorts Haskell language elements in views by name.</p>
  *
  * @author Leif Frenzel
  */
class HaskellLanguageElementSorter extends ViewerSorter {


  // interface methods of ViewerSorter
  ////////////////////////////////////

  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
    return compare( ( IHaskellLanguageElement )e1,
                    ( IHaskellLanguageElement )e2 );
  }

  @Override
  public boolean isSorterProperty( final Object element, final String prop ) {
    return true;
  }


  // helping methods
  //////////////////

  @SuppressWarnings("unchecked")
  private int compare( final IHaskellLanguageElement elem1,
                       final IHaskellLanguageElement elem2 ) {
    return getComparator().compare( elem1.getName(), elem2.getName() );
  }
}