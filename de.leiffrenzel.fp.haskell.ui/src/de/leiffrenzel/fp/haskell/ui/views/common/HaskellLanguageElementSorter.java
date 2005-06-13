// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.views.common;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

import de.leiffrenzel.fp.haskell.core.halamo.IHaskellLanguageElement;

/** <p>sorts Haskell language elements in views by name.</p>
  * 
  * @author Leif Frenzel
  */
class HaskellLanguageElementSorter extends ViewerSorter {
 
  
  // interface methods of ViewerSorter
  ////////////////////////////////////
  
  public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
    return compare( ( IHaskellLanguageElement )e1, 
                    ( IHaskellLanguageElement )e2 );
  }

  public boolean isSorterProperty( final Object element, final String prop ) {
    return true;
  }

  
  // helping methods
  //////////////////
  
  private int compare( final IHaskellLanguageElement elem1, 
                       final IHaskellLanguageElement elem2 ) {
    return collator.compare( elem1.getName(), elem2.getName() );
  }
}