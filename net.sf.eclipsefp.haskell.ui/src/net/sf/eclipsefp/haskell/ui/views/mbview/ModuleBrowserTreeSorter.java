// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.viewers.ViewerSorter;


/** <p>sorts the levels of the module browser's tree.</p>
  * 
  * @author Leif Frenzel
  */
public class ModuleBrowserTreeSorter extends ViewerSorter {

  private static final int CATEGORY_FOLDER = 1;
  private static final int CATEGORY_FILE   = 2;
  
  public int category( final Object element ) {
    int result = super.category( element );
    if( element instanceof IFolder ) {
      result = CATEGORY_FOLDER;
    } else {
      result = CATEGORY_FILE;
    }
    return result;
  }
}
