package net.sf.eclipsefp.haskell.browser.views;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


public class NoDatabaseContentProvider implements ITreeContentProvider {

  public void dispose() {
    // Do nothing
  }

  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
    // Do nothing
  }

  public Object[] getElements( final Object inputElement ) {
    return new Object[] { NoDatabaseRoot.ROOT };
  }

  public Object[] getChildren( final Object parentElement ) {
    return new Object[0];
  }

  public Object getParent( final Object element ) {
    return null;
  }

  public boolean hasChildren( final Object element ) {
    return false;
  }

}
