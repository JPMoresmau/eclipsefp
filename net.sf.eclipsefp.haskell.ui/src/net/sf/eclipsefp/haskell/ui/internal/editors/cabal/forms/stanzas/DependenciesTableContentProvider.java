/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.List;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Content provider for dependencies edition.
 * @author Alejandro Serrano
 *
 */
public class DependenciesTableContentProvider implements
    IStructuredContentProvider {

  List<DependencyItem> items;

  @Override
  public void dispose() {
    // Do nothing
  }

  @Override
  @SuppressWarnings ( "unchecked" )
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
    items = (List<DependencyItem>)newInput;
  }

  @Override
  public Object[] getElements( final Object inputElement ) {
    return items.toArray();
  }
}
