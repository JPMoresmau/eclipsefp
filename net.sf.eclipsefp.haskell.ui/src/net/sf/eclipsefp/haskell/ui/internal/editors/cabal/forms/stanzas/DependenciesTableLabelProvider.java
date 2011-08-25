/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * Provider for the dependencies editor, allowing to chane the version.
 * @author Alejandro Serrano
 *
 */
public class DependenciesTableLabelProvider implements ITableLabelProvider {

  public void dispose() {
    // Do nothing
  }

  public boolean isLabelProperty( final Object element, final String property ) {
    return (property.equals("package") || property.equals("version")); // $NON-NLS-1
  }

  public Image getColumnImage( final Object element, final int columnIndex ) {
    return null;
  }

  public String getColumnText( final Object element, final int columnIndex ) {
    DependencyItem item = (DependencyItem)element;
    if (columnIndex == 0) {
      return item.getPackage();
    } else if (columnIndex == 1) {
      return item.getVersion();
    } else {
      return null;
    }
  }

  public void addListener( final ILabelProviderListener listener ) {
    // Do nothing
  }

  public void removeListener( final ILabelProviderListener listener ) {
    // Do nothing
  }

}
