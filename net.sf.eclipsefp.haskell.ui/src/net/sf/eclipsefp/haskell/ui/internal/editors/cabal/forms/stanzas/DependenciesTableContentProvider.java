package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.List;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;


public class DependenciesTableContentProvider implements
    IStructuredContentProvider {

  List<DependencyItem> items;

  public void dispose() {
    // Do nothing
  }

  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
    items = (List<DependencyItem>)newInput;
  }

  public Object[] getElements( final Object inputElement ) {
    return items.toArray();
  }
}
