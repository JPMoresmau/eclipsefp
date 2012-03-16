// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.common;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/** <p>content provider for generic trees consisting of ITreeElements.</p>
  *
  * @author Leif Frenzel
  */
public class TreeElementCP implements ITreeContentProvider {

  // interface methods of ITreeContentProvider
  ////////////////////////////////////////////

  @Override
  public Object[] getChildren( final Object parentElement ) {
    Object[] result = new Object[ 0 ];
    if( parentElement instanceof ITreeElement ) {
      ITreeElement element = ( ITreeElement )parentElement;
      result = element.getChildren().toArray();
    }
    return result;
  }

  @Override
  public Object getParent( final Object element ) {
    Object result = null;
    if( element instanceof ITreeElement ) {
      result = ( ( ITreeElement )element ).getParent();
    }
    return result;
  }

  @Override
  public boolean hasChildren( final Object element ) {
    Object[] children = getChildren( element );
    return children != null && children.length > 0;
  }

  @Override
  public Object[] getElements( final Object inputElement ) {
    return getChildren( inputElement );
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
